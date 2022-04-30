{ config, pkgs, lib, ... }:

{

  imports = [
    ./home-headless.nix
    # ./home-desktop.nix
    # ./home-chromeos.nix
    # ./home-linux.nix
  ];

}
