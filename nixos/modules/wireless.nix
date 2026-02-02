{ pkgs, lib, config, ... }:

let

  networks = config.me.private.wireless.networks;

  script-line = ssid: network: ''
    test "$1" = "${ssid}" && ${pkgs.qrencode}/bin/qrencode -o - -t ANSI "WIFI:S:${ssid};T:WPA;P:${network.psk};;"
  '';

  psk-networks = lib.filterAttrs (_: v: builtins.hasAttr "psk" v) networks;
  script = builtins.concatStringsSep "\n" (lib.mapAttrsToList script-line psk-networks);

  qr = pkgs.writeShellScriptBin "qr" ''
    set -ue
    ${script}
  '';


in

{
  networking.wireless = {
    enable = true;
    interfaces = [ config.me.wifi-interface ];
    inherit networks;
    userControlled = true;
  };

  users.users.me.packages = [ qr ];
}
