{ config, pkgs, lib, ... }:

let

  router = (import ./private.nix).router;
  ssid = router.ssid;
  wpaPassphrase = router.wpaPassphrase;
  countryCode = router.countryCode;

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
    interface = "wlan0";
    hwMode = "g";
    # hwMode = "a";
    # channel = 46;
    inherit ssid wpaPassphrase;
    extraConfig = ''
      country_code=${countryCode}
      ieee80211d=1
      wpa=2
      auth_algs=1
      wpa_key_mgmt=WPA-PSK
      wpa_pairwise=CCMP
      rsn_pairwise=CCMP
    '';
  };

  services.dnsmasq = {
    enable = true;
    extraConfig = ''
      interface=wlan0
      bind-interfaces
      dhcp-range=172.18.18.10,172.18.18.50,24h
    '';
  };

  # services.nscd.enable = false;

  networking = {
    interfaces.wlan0.ipv4.addresses = [{
      address = "172.18.18.1";
      prefixLength = 24;
    }];
    interfaces.eth0.ipv4.addresses = [{
      address = "192.168.1.44";
      prefixLength = 24;
    }];
    defaultGateway = {
      address = "192.168.1.254";
    };
    nameservers = [ "8.8.8.8" ];
    dhcpcd.enable = false;
    useDHCP = false;
    # useNetworkd = true;
    nat = {
      enable = true;
      internalInterfaces = [ "wlan0" ];
      # internalIPs = [ "172.18.18.1/24" ];
      externalInterface = "eth0";
      extraCommands = ''
        iptables -A FORWARD -d 10.0.0.0/8 -j DROP
        # iptables -A FORWARD -d 172.16.0.0/12 -j DROP
        iptables -A FORWARD -d 192.168.1.0/16 -j DROP
      '';
    };
    firewall = {
      enable = true;
      interfaces.wlan0.allowedUDPPorts = [ 53 67 ];
      allowedTCPPorts = [ 22 ];
    };
  };
}
