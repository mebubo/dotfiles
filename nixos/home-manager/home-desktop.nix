{ config, pkgs, lib, ... }:

let

  obsidian = pkgs.callPackage ../packages/obsidian.nix {};

in

{

  home.packages = with pkgs; [
    cage
    chromium
    dmenu
    i3
    i3lock
    i3status
    st
    pulseaudio
    pavucontrol
    wlr-randr
    tlaplus
    grim
    slurp
    obsidian
  ];

  home.pointerCursor = {
    gtk.enable = true;
    # package = pkgs.vanilla-dmz;
    package = pkgs.apple-cursor;
    # name = "Vanilla-DMZ";
    name = "macOS-Monterey-White";
    size = 20;
  };

  programs = {
    firefox = {
      enable = true;
      package = pkgs.firefox-wayland;
      profiles = {
        me = {
          userChrome = ''
            #tabbrowser-tabs {
              visibility: collapse !important;
            }
          '';
        };
      };
    };
    alacritty = {
      enable = true;
      settings = {
        font = {
          size = 9;
        };
      };
    };
    foot = {
      enable = true;
    };
    obs-studio = {
      enable = true;
      plugins = [
        # pkgs.obs-studio-plugins.wlrobs
      ];
    };

    vscode = import ./vscode.nix pkgs pkgs.vscode;

  };

  xdg.configFile = {
    "sway/config".source = ../../.config/sway/config;
    "i3/config".source = ../../.config/i3/config;
    "i3status/config".source = ../../.config/i3status/config;
    "mpv/mpv.conf".source = ../../.config/mpv/mpv.conf;
  };

  targets.genericLinux.enable = lib.mkForce false;

}
