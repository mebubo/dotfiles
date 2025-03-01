{ config, pkgs, lib, osConfig, ... }:

let

my-vscode =

  let
    version = "1.97.2";
    sha256 = "sha256-wXYD5gKSenkhheTenkQx3gB8lAGY2ru7YLWO3z7wQIU=";
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
    my-vscode
    zed-editor.fhs
    poetry
    uv
    ruff
    deno
    gradle
    files-to-prompt
    aichat
    repomix
  ];

  programs = {
    bash = {
      sessionVariables = {
        JAVA_HOME = pkgs.jdk;
        LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/";
      };
    };
  };

}
