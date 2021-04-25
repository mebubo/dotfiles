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
    networks = (import ./private.nix).wireless.networks;
    userControlled = {
      enable = true;
      group = "wireless";
    };
  };

  users.users.me.packages = [ qr ];
}
