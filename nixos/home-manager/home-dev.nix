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
    metals
    scala-cli
    ctags
    nodejs
    openjdk
    sbt
    tlaplus
    pipx
    aider-chat
    code-cursor
    zed-editor.fhs
  ];

  programs = {
    vscode = import ./vscode.nix pkgs pkgs.vscode;
    bash = {
      sessionVariables = {
        JAVA_HOME = pkgs.jdk;
        LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/";
      };
    };
  };

}
