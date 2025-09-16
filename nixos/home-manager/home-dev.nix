{ config, pkgs, lib, ... }:

let

dev-home-manager-rebuild = pkgs.writeShellScriptBin "dev-home-manager-rebuild" ''
  set -eu
  nix build '.#homeConfigurations.dev.activationPackage'
  result/activate
'';

my-vscode =

  let
    version = "1.104.0";
    sha256 = "sha256-ABnbLiF8AKClsGjhHb/yK4jD3Tt8y/NdbxicSkp+Hbs=";
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

    version = "1.6.23";
    url = "https://downloads.cursor.com/production/9b5f3f4f2368631e3455d37672ca61b6dce8543e/linux/x64/Cursor-1.6.23-x86_64.AppImage";
    hash = "sha256-DyWy8c+H1vhX6OGo8m78To/wExFmZ0eNeHk6YmSuOe0=";

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
    dev-home-manager-rebuild
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
