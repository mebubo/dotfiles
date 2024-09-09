{ config, pkgs, lib, osConfig, ... }:

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
    # obsidian
  ];

  home.pointerCursor = {
    gtk.enable = true;
    # package = pkgs.vanilla-dmz;
    package = pkgs.adwaita-icon-theme;
    # package = pkgs.apple-cursor;
    # name = "Vanilla-DMZ";
    name = "Adwaita";
    # name = "macOS-Monterey-White";
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

    i3status-rust = {
      enable = true;
      bars = {
        bottom = {
          blocks = [
            {
              block = "net";
              device = "^wlp.*";
              format = " $ssid $signal_strength $ip ";
            }
            {
              block = "cpu";
              interval = 1;
            }
            {
              block = "temperature";
              format = " $icon $average $max ";
            }
            {
              block = "backlight";
            }
            {
              block = "bluetooth";
              format = " $icon $percentage ";
              disconnected_format = " $icon ";
              mac = osConfig.me.private.bluetooth.headset;
            }
            {
              block = "bluetooth";
              format = " $icon $percentage ";
              disconnected_format = " $icon ";
              mac = osConfig.me.private.bluetooth.mouse;
            }
            {
              block = "battery";
              format = " $icon $percentage $power $time ";
            }
            {
              block = "sound";
            }
            {
              block = "keyboard_layout";
              driver = "sway";
              format = " $layout ";
              mappings = {
                "English (US)" = "en";
                "French (N/A)" = "fr";
                "Russian (phonetic)" = "ru";
              };
            }
            {
              block = "time";
              format = "$timestamp.datetime(format:' %a %Y-%m-%d %H:%M:%S ')";
              interval = 1;
            }
          ];
          # settings = { };
          icons = "emoji";
          theme = "native";
        };
      };
    };

  };

  xdg.configFile = {
    "sway/config".source = ../../.config/sway/config;
    # "i3/config".source = ../../.config/i3/config;
    "i3status/config".source = ../../.config/i3status/config;
    "mpv/mpv.conf".source = ../../.config/mpv/mpv.conf;
  };

  targets.genericLinux.enable = lib.mkForce false;

}
