{ config, pkgs, lib, osConfig, ... }:

let

my-vscode =

  let
    version = "1.100.1";
    sha256 = "sha256-PR8ot4tiklAYZbF7zURuRqqM9dT+PGQ7D+ljEKrfCB8=";
  in
    pkgs.vscode.overrideAttrs (oldAttrs: {
      inherit version;
      src = pkgs.fetchurl {
        name = "VSCode_${version}_linux-x64.tar.gz";
        url = "https://update.code.visualstudio.com/${version}/linux-x64/stable";
        inherit sha256;
      };
    });

my-cursor =

  let

    version = "0.50.5";
    url = "https://downloads.cursor.com/production/96e5b01ca25f8fbd4c4c10bc69b15f6228c80771/linux/x64/Cursor-0.50.5-x86_64.AppImage";
    hash = "sha256-DUWIgQYD3Wj6hF7NBb00OGRynKmXcFldWFUA6W8CZeM=";

  in

    pkgs.callPackage ../packages/code-cursor/package.nix {
      sourcesOverride = {
        x86_64-linux = pkgs.fetchurl {
          inherit url hash;
        };
      };
      versionOverride = version;
    };

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
    bun
    openjdk
    sbt
    tlaplus
    pipx
    my-vscode
    zed-editor.fhs
    # poetry
    uv
    ruff
    deno
    gradle
    files-to-prompt
    aichat
    repomix
    my-cursor
    mise
    # goose-cli
    # claude-code
    windsurf
  ];

  programs = {
    bash = {
      sessionVariables = {
        JAVA_HOME = pkgs.jdk;
        # LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/";
      };
    };
  };

}
