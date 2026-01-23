{ config, pkgs, lib, ... }:

let

  router = config.me.private.router;
  ssid = router.ssid;
  wpaPassphrase = router.wpaPassphrase;
  countryCode = router.countryCode;
  apAddress = "10.10.10.1";
  apDhcpRange = "10.10.10.100,10.10.10.200,24h";
  upstreamRouter = "192.168.1.254";

  qr = pkgs.writeShellScriptBin "qr" ''
    ${pkgs.qrencode}/bin/qrencode -o - -t ANSI "WIFI:S:${ssid};T:WPA;P:${wpaPassphrase};;"
  '';
in

{

  environment.systemPackages = with pkgs; [
    qr
  ];

  services.hostapd = {
    enable = true;
    radios.wlan0 = {
      band = "2g";
      inherit countryCode;
      wifi4.enable = true;
      wifi5.enable = false;
      networks.wlan0 = {
        inherit ssid;
        authentication = {
          mode = "wpa2-sha256";
          wpaPassword = wpaPassphrase;
        };
      };
    };
  };

  services.dnsmasq = {
    enable = true;
    settings = {
      interface = "wlan0";
      bind-interfaces = true;
      dhcp-range = apDhcpRange;
      dhcp-option = [
        "option:router,${apAddress}"
        "option:dns-server,${apAddress}"
      ];
      no-resolv = true;
    };
  };

  # services.nscd.enable = false;

  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };

  networking = {
    interfaces.wlan0.ipv4.addresses = [{
      address = apAddress;
      prefixLength = 24;
    }];
    interfaces.eth0.ipv4.addresses = [{
      address = "192.168.1.44";
      prefixLength = 24;
    }];
    defaultGateway = {
      address = upstreamRouter;
    };
    nameservers = [ "8.8.8.8" ];
    dhcpcd.enable = false;
    useDHCP = false;
  };

  networking.nftables = {
    enable = true;
    ruleset = ''
      table inet filter {
        chain input {
          type filter hook input priority 0; policy drop;

          iif "lo" accept
          ct state established,related accept

          # Allow DHCP and DNS from WiFi clients
          iifname "wlan0" udp dport { 53, 67 } accept

          # Allow SSH (adjust as needed)
          tcp dport 22 accept

          # Allow ping
          icmp type echo-request accept
        }

        chain forward {
          type filter hook forward priority 0; policy drop;

          ct state established,related accept

          # Block DNS to the upstream router
          iifname "wlan0" ip daddr ${upstreamRouter} udp dport 53 drop
          iifname "wlan0" ip daddr ${upstreamRouter} tcp dport 53 drop

          # Allow WiFi clients to reach the home network (RFC1918 ranges)
          iifname "wlan0" ip daddr 192.168.0.0/16 accept
          iifname "wlan0" ip daddr 10.0.0.0/8 accept
          iifname "wlan0" ip daddr 172.16.0.0/12 accept

          # Everything else from wlan0 is dropped (no internet)
        }

        chain output {
          type filter hook output priority 0; policy accept;
        }
      }

      table inet nat {
        chain postrouting {
          type nat hook postrouting priority 100;

          # NAT for traffic going to home network
          oifname "eth0" masquerade
        }
      }
    '';
  };

  # Disable default firewall since we're using nftables directly
  networking.firewall.enable = false;
}
