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
    networks = {
      "${ssid}" = {
        inherit psk;
      };
    };
  };

  users.users.me.packages = [ qr ];
}
