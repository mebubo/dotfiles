{ config, pkgs, lib, ... }:

let

screenshot-copy = pkgs.writeShellScriptBin "screenshot-copy" ''
    ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.wl-clipboard}/bin/wl-copy
'';

in

{

  home.packages = with pkgs; [
    cage
    chromium
    google-chrome
    pulseaudio
    pavucontrol
    screenshot-copy
    ghostty
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
    bash = {
      sessionVariables = {
        NIXOS_OZONE_WL = "1";
      };
    };
    ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks = {
        "dev" = {
          hostname = "localhost";
          user = "dev";
        };
        "doc" = {
          hostname = "localhost";
          user = "doc";
        };
        "game" = {
          hostname = "localhost";
          user = "game";
        };
        "scr" = {
          hostname = "localhost";
          user = "scr";
        };
      };
    };
    firefox = {
      enable = true;
      package = pkgs.firefox;
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
      settings = {
        main = {
          # dpi-aware = "yes";
          word-delimiters = "`|";
          font = "CaskaydiaMono Nerd Font:size=9";
        };
      };
    };
    obs-studio = {
      enable = true;
      plugins = [
        # pkgs.obs-studio-plugins.wlrobs
      ];
    };
    i3status-rust = {
      enable = true;
      bars = {
        bottom = {
          blocks = [
            {
              block = "music";
              format = " $icon {$combo|} ";
            }
            {
              block = "net";
              device = "^wlp.*";
              format = " {$ssid $signal_strength $bitrate.eng(w:3) $ip|Disconnected} ";
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
              block = "battery";
              format = " $icon $percentage $power $time ";
              full_format = " $icon {$percentage|} {$power|} {$time|} ";
              charging_format = " $icon {$percentage|} {$power|} {$time|} ";
              empty_format = " $icon {$percentage|} {$power|} {$time|} ";
              not_charging_format = " $icon {$percentage|} {$power|} {$time|} ";
              missing_format = " $icon {$percentage|} {$power|} {$time|} ";
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

  services.mpris-proxy.enable = true;

  services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "undocked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
            status = "enable";
          }
        ];
      }
      {
        profile.name = "docked3";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "DP-3";
            scale = 1.0;
            status = "enable";
          }
        ];
      }
      {
        profile.name = "docked4";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "DP-4";
            scale = 1.0;
            status = "enable";
          }
        ];
      }
    ];
  };

  services.hypridle = {
    enable = true;
    settings = {
      general = {
        lock_cmd = "hyprlock";
        # before_sleep_cmd = "loginctl lock-session";
        before_sleep_cmd = "hyprlock";
        after_sleep_cmd = "hyprctl dispatch dpms on";
        inhibit_sleep = 3;
      };
    };
  };

  systemd.user.services.wayland-share-permissions = {
    Unit = {
      Description = "Set Wayland socket permissions for group access";
      After = [ "graphical-session.target" ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "wayland-permissions" ''
        while [ ! -S "$XDG_RUNTIME_DIR/wayland-1" ]; do
          ${pkgs.coreutils}/bin/sleep 1
        done

        ${pkgs.coreutils}/bin/chmod g+rx "$XDG_RUNTIME_DIR"
        ${pkgs.coreutils}/bin/chmod g+rwx "$XDG_RUNTIME_DIR/wayland-1"
        ${pkgs.coreutils}/bin/chmod g+rw "$XDG_RUNTIME_DIR/wayland-1.lock"
      '';
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
