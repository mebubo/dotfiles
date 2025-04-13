{ config, pkgs, lib, osConfig, ... }:

let

my-vscode =

  let
    version = "1.98.2";
    sha256 = "sha256-YRzzQOB2g2gOoucH5kU7yVyT51rEONpKbmZgp6S8sl4=";
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

    version = "0.48.9";
    url = "https://downloads.cursor.com/production/61e99179e4080fecf9d8b92c6e2e3e00fbfb53f4/linux/x64/Cursor-0.48.9-x86_64.AppImage";
    hash = "sha256-Rw96CIN+vL1bIj5o68gWkHeiqgxExzbjwcW4ad10M2I=";

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
    claude-code
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
