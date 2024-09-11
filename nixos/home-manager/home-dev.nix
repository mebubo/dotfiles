{ config, pkgs, lib, osConfig, ... }:


{

  home.packages = with pkgs; [
    alloy
    coursier
    ctags
    nodejs
    openjdk
    sbt
    tlaplus
  ];

  programs = {
    vscode = import ./vscode.nix pkgs pkgs.vscode;
  };

}
