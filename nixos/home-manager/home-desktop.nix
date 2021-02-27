{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    cage
    chromium
    dmenu
    google-chrome
    i3
    i3lock
    i3status
    pavucontrol
    wlr-randr
  ];

  programs = {
    firefox = {
      enable = false;
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
    obs-studio = {
      enable = true;
      plugins = [ pkgs.obs-wlrobs pkgs.obs-v4l2sink ];
    };
  };

  xdg.configFile = {
    "sway/config".source = ../../.config/sway/config;
    "i3/config".source = ../../.config/i3/config;
    "i3status/config".source = ../../.config/i3status/config;
    "youtube-dl/config".source = ../../.config/youtube-dl/config;
    "mpv/mpv.conf".source = ../../.config/mpv/mpv.conf;
  };

  targets.genericLinux.enable = lib.mkForce false;

  nixpkgs.overlays = [
    (self: super: {
      chromium = super.chromium.override {
        enableVaapi = true;
      };
    })
  ];
}
