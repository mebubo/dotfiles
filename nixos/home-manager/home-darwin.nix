{ config, pkgs, lib, ... }:

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
  ];

  home = {
    username = "me";
    homeDirectory = lib.mkForce "/Users/me";
    file = {
      ".config/captive-browser.toml".source = ../../.config/captive-browser-mac-chrome.toml;
    };
  };

  programs = {

    bash = {
      enable = true;
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
