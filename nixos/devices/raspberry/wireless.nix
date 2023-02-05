{ pkgs, ... }:

{
  networking.wireless = {
    enable = true;
    interfaces = [ "wlan0" ];
    networks = (import ./private.nix).wireless.networks;
  };

  networking = {
    interfaces.wlan0.ipv4.addresses = [{
      address = "192.168.1.123";
      prefixLength = 24;
    }];
    defaultGateway = "192.168.1.254";
    nameservers = [ "8.8.8.8" ];
    dhcpcd.enable = false;
  };

}
