{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    openssh
    jetbrains.idea-community-minimal
    qemu
    podman
    m-cli
    alacritty
    utm
  ];

  home = {
    username = "me";
    homeDirectory = lib.mkForce "/Users/me";
  };

  programs = {

    vscode = import ./vscode.nix pkgs pkgs.vscode;

    bash = {
      initExtra = ''
        export PATH=/Users/me/.nix-profile/bin:/etc/profiles/per-user/me/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:$PATH
      '';
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
  };

  targets.genericLinux.enable = lib.mkForce false;
}
