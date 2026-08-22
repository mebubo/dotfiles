{ config, pkgs, lib, ... }:

let

my-vscode =

  let
    version = "1.134.0";
    sha256 = "sha256-kzYXAHn1J/HKYtrTmUewINsGvT6Mrn4eYJe+pZTzdzM=";
  in
    pkgs.vscode.overrideAttrs (oldAttrs: {
      inherit version;
      src = pkgs.fetchurl {
        name = "VSCode_${version}_linux-x64.tar.gz";
        url = "https://update.code.visualstudio.com/${version}/linux-x64/stable";
        inherit sha256;
      };
    });

dotfile = path: config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/src/me/dotfiles/${path}";

in

{

  xdg.configFile = {
    "herdr/config.toml".source = dotfile ".config/herdr/config.toml";
  };

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
    bubblewrap
    socat
    my-vscode
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
    herdr = {
      enable = true;
    };
  };

}
