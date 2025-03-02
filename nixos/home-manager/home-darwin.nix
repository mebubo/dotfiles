{ config, pkgs, lib, ... }:

let

username = "me";

in

{

  imports = [
    ./lima
  ];

  home.packages = with pkgs; [
    openssh
    coreutils
    jetbrains.idea-community-minimal
    qemu
    podman
    m-cli
    utm
    obsidian
    captive-browser
    anki-bin
    vscode
    code-cursor
    zed-editor
    coursier
    scala-cli
    nodejs
    openjdk
    sbt
    pipx
    uv
    ruff
    deno
    gradle
    files-to-prompt
    aichat
    repomix
    google-chrome
    bun
    monaspace
    aerospace
    discrete-scroll
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkForce "/Users/${username}";
    file = {
      ".config/captive-browser.toml".source = ../../.config/captive-browser-mac-chrome.toml;
    };
  };

  programs = {

    bash = {
      enable = true;
      initExtra = ''
        export PATH=$PATH:~/.local/bin
      '';
    };

    alacritty = {
      enable = true;
      settings = {
        window = {
          option_as_alt = "OnlyLeft";
        };
        font = {
          size = 12;
          normal = {
            family = "Monaspace Neon";
            style = "Regular";
          };
        };
      };
    };
    ssh = {
      enable = true;
      matchBlocks = {
        "dev" = {
          hostname = "localhost";
          user = "dev";
        };
      };
    };
  };

  targets.genericLinux.enable = lib.mkForce false;
}
