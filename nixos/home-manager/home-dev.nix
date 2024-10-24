{ config, pkgs, lib, osConfig, ... }:

let

vscode =

  let
    version = "1.94.2";
    sha256 = "sha256-NktZowxWnt96Xa4Yxyv+oMmwHGylYIxFrpws/y0XhXA=";
  in
    pkgs.vscode.overrideAttrs (oldAttrs: {
      inherit version;
      src = pkgs.fetchurl {
        name = "VSCode_${version}_linux-x64.tar.gz";
        url = "https://update.code.visualstudio.com/${version}/linux-x64/stable";
        inherit sha256;
      };
    });

in

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
    # aider-chat
    # code-cursor
    zed-editor.fhs
    poetry
    uv
    ruff
    deno
  ];

  programs = {
    vscode = import ./vscode.nix pkgs vscode;
    bash = {
      sessionVariables = {
        JAVA_HOME = pkgs.jdk;
        LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/";
      };
    };
  };

}
