{ pkgs, ... }:

let

  lima-create-default = pkgs.callPackage ./lima.nix {};

in

{

  home.packages = with pkgs; [
    lima
    lima-create-default
  ];

  home.file = {
    ".ssh/config".source = ./ssh-config;
  };

}
