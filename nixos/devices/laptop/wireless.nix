{ dotfiles-private }:
{ pkgs, ... }:

let
  ssid = "ssid";
  psk = "psk";

  qr = pkgs.writeShellScriptBin "qr" ''
    ${pkgs.qrencode}/bin/qrencode -o - -t ANSI "WIFI:S:${ssid};T:WPA;P:${psk};;"
  '';


in

{
  networking.wireless = {
    enable = true;
    interfaces = [ "wlp3s0" ];
    networks = (import dotfiles-private).wireless.networks;
    userControlled = {
      enable = true;
      group = "wireless";
    };
  };

  users.users.me.packages = [ qr ];
}
