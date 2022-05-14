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
    vscode
    jetbrains.idea-community-minimal
    qemu
    podman
    m-cli
    alacritty
  ];

  programs = {
    zsh = {
      enable = true;
      loginExtra = ''
        exec ${pkgs.bashInteractive}/bin/bash -l
      '';
      enableCompletion = false;
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
