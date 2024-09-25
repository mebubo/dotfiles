{ config, pkgs, lib, osConfig, ... }:

let

  # obsidian = pkgs.callPackage ../packages/obsidian.nix {};

in

{

  home.packages = with pkgs; [
    obsidian
    anki-bin
    foliate
    maestral
    maestral-gui
  ];

  programs = {
    sioyek = {
      enable = true;
    };
  };

}
