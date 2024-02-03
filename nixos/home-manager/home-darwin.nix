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
  ];

  home = {
    username = "me";
    homeDirectory = lib.mkForce "/Users/me";
    file = {
      ".config/captive-browser.toml".source = ../../.config/captive-browser-mac-chrome.toml;
    };
  };

  programs = {

    vscode = import ./vscode.nix pkgs pkgs.vscode;

    bash = {
      initExtra = ''
        export PATH=/Users/me/.nix-profile/bin:/etc/profiles/per-user/me/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:$PATH
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
  };

  targets.genericLinux.enable = lib.mkForce false;
}
