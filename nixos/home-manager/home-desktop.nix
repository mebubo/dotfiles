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
    pavucontrol
    wlr-randr
    (wrapOBS {
      plugins = with obs-studio-plugins; [
        wlrobs
      ];
    })
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
        colors = {
          primary = {
            background = "0xfdf6e3";
            foreground = "0x586e75";
          };

          normal = {
            black =   "0x073642";
            red =     "0xdc322f";
            green =   "0x859900";
            yellow =  "0xb58900";
            blue =    "0x268bd2";
            magenta = "0xd33682";
            cyan =    "0x2aa198";
            white =   "0xeee8d5";
          };

          bright = {
            black =   "0x002b36";
            red =     "0xcb4b16";
            green =   "0x586e75";
            yellow =  "0x657b83";
            blue =    "0x839496";
            magenta = "0x6c71c4";
            cyan =    "0x93a1a1";
            white =   "0xfdf6e3";
          };
        };
      };
    };
    # obs-studio = {
    #   enable = true;
    #   plugins = [ pkgs.obs-wlrobs pkgs.obs-v4l2sink ];
    # };
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
        commandLineArgs = [
          "--enable-features=VaapiVideoDecoder"
          "--enable-features=UseOzonePlatform" "--ozone-platform=wayland"
        ];
      };
    })
    (import ../overlays/50-st)
  ];
}
