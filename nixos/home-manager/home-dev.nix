{ config, pkgs, lib, osConfig, ... }:


{

  home.packages = with pkgs; [
    cmake
    ninja
    gnumake
    pkg-config
    gcc
    alloy
    coursier
    ctags
    nodejs
    openjdk
    sbt
    tlaplus
    pipx
    aider-chat
  ];

  programs = {
    vscode = import ./vscode.nix pkgs pkgs.vscode;
  };

}
