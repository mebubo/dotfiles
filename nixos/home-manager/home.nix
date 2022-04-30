{ config, pkgs, lib, ... }:

{

  imports = [
    ./home-headless.nix
    # ./home-desktop.nix
    # ./home-chromeos.nix
    # ./home-linux.nix
    # ./home-darwin.nix
  ];

  nixpkgs.overlays = [
    (import ../overlays/50-haskell.nix)
    (import ../overlays/50-home-manager.nix)
    (import ../overlays/50-vim-plugins.nix)
    (import ../overlays/50-chromium.nix)
    (import ../overlays/50-st)
    (import ../overlays/50-intellij.nix)
  ];

}
