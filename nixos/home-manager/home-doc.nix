{ config, pkgs, lib, ... }:

let

  # obsidian = pkgs.callPackage ../packages/obsidian.nix {};

in

{

  home.packages = with pkgs; [
    obsidian
    anki-bin
    foliate
    maestral
    koreader
  ];

  programs = {
    sioyek = {
      enable = true;
      config = {
        "background_color" = "0.0 0.0 0.0";
      };
    };
  };

}
