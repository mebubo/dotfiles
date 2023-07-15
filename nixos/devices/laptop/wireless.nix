{ dotfiles-private }:
{ pkgs, lib, ... }:

let

  networks = (import dotfiles-private).wireless.networks;

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
    interfaces = [ "wlp3s0" ];
    inherit networks;
    userControlled = {
      enable = true;
      group = "wireless";
    };
  };

  users.users.me.packages = [ qr ];
}
