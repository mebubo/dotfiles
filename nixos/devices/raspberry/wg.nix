{ config, pkgs, lib, ... }:

{

  networking = {
    wireguard = {
      enable = true;
      interfaces = {
        wg0 = {
          ips = [ "10.100.0.1/24" ];
          listenPort = 51820;
          postSetup = ''
            ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
          '';
          postShutdown = ''
            ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
          '';

          privateKeyFile = "/home/me/wireguard-keys/private";

          peers = [
            {
              publicKey = "...";
              allowedIPs = [ "10.100.0.2/32" ];
            }
          ];
        };
      };
    };
    nat = {
      enable = true;
      externalInterface = "eth0";
      internalInterfaces = [ "wg0" ];
    };
    firewall = {
      enable = true;
      allowedUDPPorts = [ 51820 ];
    };
  };
}
