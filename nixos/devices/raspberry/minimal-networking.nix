{ ... }:

{

  networking = {
    interfaces.eth0.ipv4.addresses = [{
      address = "192.168.1.122";
      prefixLength = 24;
    }];
    defaultGateway = "192.168.1.254";
    nameservers = [ "8.8.8.8" ];
    dhcpcd.enable = false;
  };


}
