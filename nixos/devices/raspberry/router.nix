{ config, pkgs, lib, ... }:

let

  router = config.me.private.router;
  ssid = router.ssid;
  wpaPassphrase = router.wpaPassphrase;
  countryCode = router.countryCode;
  apAddress = "10.10.10.1";
  apDhcpRange = "10.10.10.100,10.10.10.200,24h";
  apSubnet = "10.10.10.0/24";
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
      channel = 1;
      wifi4.enable = false;
      wifi5.enable = false;
      settings = {
        country_code = countryCode;
        ieee80211d = true;
      };
      networks.wlan0 = {
        inherit ssid;
        authentication = {
          mode = "wpa2-sha1";
          wpaPassword = wpaPassphrase;
          pairwiseCiphers = [ "CCMP" ];
        };
        settings = {
          auth_algs = 1;
        };
      };
    };
  };

  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;
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
      address = "192.168.1.122";
      prefixLength = 24;
    }];
    defaultGateway = {
      address = upstreamRouter;
    };
    nameservers = [ "8.8.8.8" ];
    dhcpcd.enable = false;
    useDHCP = false;
  };

  networking.firewall = {
    enable = true;
    backend = "nftables";
    filterForward = true;
    interfaces.wlan0.allowedUDPPorts = [ 53 67 ];
    allowedTCPPorts = [ 22 ];
    extraForwardRules = ''
      # Block DNS to the upstream router
      iifname "wlan0" ip daddr ${upstreamRouter} udp dport 53 drop
      iifname "wlan0" ip daddr ${upstreamRouter} tcp dport 53 drop

      # Allow WiFi clients to reach the home network (RFC1918 ranges)
      iifname "wlan0" ip daddr 192.168.0.0/16 accept
      iifname "wlan0" ip daddr 10.0.0.0/8 accept
      iifname "wlan0" ip daddr 172.16.0.0/12 accept
    '';
  };

  networking.nftables = {
    enable = true;
    tables."router-nat" = {
      family = "ip";
      content = ''
        chain postrouting {
          type nat hook postrouting priority 100;

          # NAT for traffic going to home network
          ip saddr ${apSubnet} oifname "eth0" masquerade
        }
      '';
    };
  };
}
