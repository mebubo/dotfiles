{ config, pkgs, lib, ... }:

let

podman-overlay = self: super: {
  podman-unwrapped = super.podman-unwrapped.overrideAttrs (attrs: {
    postPatch = ''
      substituteInPlace pkg/machine/qemu/options_darwin_arm64.go --replace /usr/local/share/qemu ${self.qemu}/share/qemu
    '';
    }
  );
};

in

{

  home.packages = with pkgs; [
    jetbrains.idea-community-minimal
    qemu
    podman
    m-cli
    alacritty
    utm
  ];

  programs = {
    zsh = {
      enable = true;
      loginExtra = ''
        exec ${pkgs.bashInteractive}/bin/bash -l
      '';
      enableCompletion = false;
    };
    alacritty = {
      enable = true;
      settings = {
        key_bindings = [
          { key = "Period"; mods = "Alt"; chars = "\\x1b."; }
          { key = "H"; mods = "Alt"; chars = "\\x1bh"; }
          { key = "J"; mods = "Alt"; chars = "\\x1bj"; }
          { key = "K"; mods = "Alt"; chars = "\\x1bk"; }
          { key = "L"; mods = "Alt"; chars = "\\x1bl"; }
          { key = "B"; mods = "Alt"; chars = "\\x1bb"; }
        ];
      };
    };
    vscode = {
      enable = true;
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;
      package = pkgs.vscode;
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

  xdg.configFile = {
    "youtube-dl/config".source = ../../.config/youtube-dl/config;
  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    podman-overlay
  ];

  targets.genericLinux.enable = lib.mkForce false;
}
