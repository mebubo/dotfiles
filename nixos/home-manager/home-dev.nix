{ config, pkgs, lib, ... }:

let

dev-home-manager-rebuild = pkgs.writeShellScriptBin "dev-home-manager-rebuild" ''
  set -eu
  nix build '.#homeConfigurations.dev.activationPackage'
  result/activate
'';

my-vscode =

  let
    version = "1.108.1";
    sha256 = "sha256-qYthiZlioD6fWCyDPfg7Yfo5PqCHzcenk8NjgobLW7c=";
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

    version = "2.4.20";
    url = "https://downloads.cursor.com/production/20b56586b0785b8843487045393c57c6d89b7103/linux/x64/Cursor-2.4.20-x86_64.AppImage";
    hash = "sha256-GshrvxQJUcDE/gOYcB2DgCZXl8jg2ZCWBOF95+4aYTE=";

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
    # windsurf
    helix
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
    helix = {
      enable = true;
      languages = {
        language-server = {
          typescript-language-server = {
            command = lib.getExe pkgs.typescript-language-server;
          };
          nil = {
            command = lib.getExe pkgs.nil;
          };
          ty = {
            command = lib.getExe pkgs.ty;
          };
          metals = {
            command = lib.getExe pkgs.metals;
          };
          harper-ls = {
            command = "${pkgs.harper}/bin/harper-ls";
          };
          markdown-oxide = {
            command = lib.getExe pkgs.markdown-oxide;
          };
        };
        language = [
        {
          name = "typescript";
          auto-format = false;
          language-servers = [ "typescript-language-server" "harper-ls" ];
        }
        {
          name = "nix";
          auto-format = false;
          language-servers = [ "nil" "harper-ls" ];
        }
        {
          name = "python";
          auto-format = false;
          language-servers = [ "ty" "harper-ls" ];
        }
        {
          name = "scala";
          auto-format = false;
          language-servers = [ "metals" "harper-ls" ];
        }
        {
          name = "markdown";
          auto-format = false;
          language-servers = [ "markdown-oxide" "harper-ls" ];
        }
        ];
      };
    };
  };

}
