pkgs: package: {
  enable = true;
  enableUpdateCheck = false;
  enableExtensionUpdateCheck = false;
  inherit package;
  extensions = with pkgs.vscode-extensions; [
    vscodevim.vim
    bbenoist.nix
    # ms-pyright.pyright
    # scalameta.metals
  ];
  mutableExtensionsDir = true;
  userSettings = {
    "editor.cursorBlinking" = "solid";
    "editor.fontSize" = 11;
    "editor.lineHeight" = 15;
    "editor.minimap.enabled" = false;
    "editor.inlineSuggest.enabled" = true;
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
    "window.restoreWindows" = "none";
    "window.titleBarStyle" = "custom";
    "workbench.activityBar.visible" = true;
    "workbench.colorTheme" = "Default Light Modern";
    "workbench.editor.showTabs" = true;
    "workbench.editor.tabCloseButton" = "right";
    "workbench.tree.indent" = 24;
    "workbench.editorAssociations" = {
      "*.md" = "vscode.markdown.preview.editor";
    };
    "typescript.updateImportsOnFileMove.enabled" = "always";
    "terminal.integrated.commandsToSkipShell" = [
      "-workbench.action.quickOpen"
    ];
  };
}
