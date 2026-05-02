{ config, pkgs, lib, ... }:

let

dev-home-manager-rebuild = pkgs.writeShellScriptBin "dev-home-manager-rebuild" ''
  set -eu
  nix build '.#homeConfigurations.dev.activationPackage'
  result/activate
'';

my-vscode =

  let
    version = "1.118.1";
    sha256 = "sha256-YF0zu0aqZpGGGaf5uNqaVaL1sMQuEVIzZ1Mczuwnfq4=";
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

    version = "3.2.16";
    url = "https://downloads.cursor.com/production/3e548838cf824b70851dd3ef27d0c6aae371b3f6/linux/x64/Cursor-3.2.16-x86_64.AppImage";
    hash = "sha256-fpiSxN53fGCb5/pWF+Hsrw4jlyomH9gOq9EjDM2TYqA=";

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
    coursier
    metals
    scala-cli
    ctags
    nodejs
    bun
    pnpm
    openjdk
    sbt
    # zed-editor.fhs
    uv
    ruff
    deno
    gradle
    mise
    my-vscode
    my-cursor
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
