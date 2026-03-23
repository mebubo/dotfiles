{ config, pkgs, lib, ... }:

let

dev-home-manager-rebuild = pkgs.writeShellScriptBin "dev-home-manager-rebuild" ''
  set -eu
  nix build '.#homeConfigurations.dev.activationPackage'
  result/activate
'';

my-vscode =

  let
    version = "1.112.0";
    sha256 = "sha256-VyjqPTyLn8eGh/XS3nn0PMqiAsrL91vDZD6Z9L2oh24=";
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

    version = "2.6.21";
    url = "https://downloads.cursor.com/production/fea2f546c979a0a4ad1deab23552a43568807592/linux/x64/Cursor-2.6.21-x86_64.AppImage";
    hash = "sha256-ZohAVWsde5fOac0JL9kc8ql/ZGohbc7S0yStARXQVSU=";

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
    uv
    ruff
    deno
    gradle
    aichat
    repomix
    my-cursor
    mise
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
