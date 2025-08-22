{ config, pkgs, lib, osConfig, ... }:

let

my-vscode =

  let
    version = "1.103.1";
    sha256 = "sha256-0zM9dyK226l4RgF1H81ojp5HC25snaN5K1QCnWIw/nw=";
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

    version = "1.5.3";
    url = "https://downloads.cursor.com/production/aece02022292184bb34fa0c899a97928489b7b78/linux/x64/Cursor-1.5.3-x86_64.AppImage";
    hash = "sha256-+PnK2wHjcew+IwqZHgROqHIdk/VgdmZ0xqn9ZUTZg9o=";

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
    pnpm
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

  home.sessionPath = [
    "$HOME/.local/bin"
  ];

  programs = {
    bash = {
      sessionVariables = {
        JAVA_HOME = "${pkgs.jdk}";
        # LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib/";
      };
    };
  };

}
