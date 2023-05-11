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
    vscode = {
      enable = true;
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions; [
        vscodevim.vim
        ms-pyright.pyright
        bbenoist.nix
        # scalameta.metals
      ];
      mutableExtensionsDir = false;
      userSettings = {
        "editor.cursorBlinking" = "solid";
        "editor.fontSize" = 11;
        "editor.lineHeight" = 15;
        "editor.minimap.enabled" = false;
        "explorer.confirmDelete" = false;
        "explorer.confirmDragAndDrop" = false;
        "extensions.autoUpdate" = false;
        "files.autoSave" = "onFocusChange";
        "files.trimTrailingWhitespace" = true;
        "telemetry.enableCrashReporter" = false;
        "telemetry.enableTelemetry" = false;
        "vim.handleKeys" = {
            "<C-k>" = false;
            "<C-t>" = false;
        };
        "vim.hlsearch" = true;
        "window.menuBarVisibility" = "classic";
        "window.restoreWindows" = "none";
        "window.titleBarStyle" = "custom";
        # "window.experimental.useSandbox" = true;
        "workbench.activityBar.visible" = true;
        # "workbench.colorTheme" = "Default Light+";
        "workbench.editor.showTabs" = false;
        "workbench.editor.tabCloseButton" = "off";
        "workbench.tree.indent" = 16;
        "typescript.updateImportsOnFileMove.enabled" = "always";
      };
    };
  };

  nixpkgs.config = {
    allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "obsidian"
    ];
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
