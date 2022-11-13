{ config, pkgs, lib, ... }:

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
    foot = {
      enable = true;
    };
    obs-studio = {
      enable = true;
      plugins = [
        pkgs.obs-studio-plugins.wlrobs
      ];
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

}
