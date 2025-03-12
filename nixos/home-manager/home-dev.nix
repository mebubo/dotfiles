{ config, pkgs, lib, osConfig, ... }:

let

my-vscode =

  let
    version = "1.98.1";
    sha256 = "sha256-+O3Pl5rlV3h/LKAfEy2yZ1MLR+3wKheuzcZVvTsIQr4=";
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

    version = "0.47.1";
    url = "https://downloads.cursor.com/production/client/linux/x64/appimage/Cursor-0.47.1-aafb3fe1326c939656bd06f325a9e17679aeec7f.deb.glibc2.25-x86_64.AppImage";
    hash = "sha256-2niWU2beJgl42m+0Fgm/Yzikb476IeV+Md5ziEuV1Qs=";

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
    poetry
    uv
    ruff
    deno
    gradle
    files-to-prompt
    aichat
    repomix
    my-cursor
    mise
    goose-cli
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
