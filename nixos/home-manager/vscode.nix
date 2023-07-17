pkgs: package: {
  enable = true;
  enableUpdateCheck = false;
  enableExtensionUpdateCheck = false;
  inherit package;
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
}
