{ config, pkgs, ... }:

let

  nixos-m1 = builtins.fetchTarball {
    url = "https://github.com/tpwrules/nixos-m1/archive/eedc13c8f9b0ca86a1bcf99104389431eb74c32a.tar.gz";
    sha256 = "0g7xxrvgh9chlg6x66qsw1iknk8zw3iv2jzfwv788qmgkyxyndv8";
  };

  m1-support = nixos-m1 + "/nix/m1-support";

in

{
  imports =
    [
      ./hardware-configuration.nix
      ./me.nix
      m1-support
      ./home-manager.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "0";
  boot.loader.efi.canTouchEfiVariables = false;

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.extraModprobeConfig = ''options v4l2loopback exclusive_caps=1 video_nr=10 card_label=my-loopback'';
  boot.kernelModules = [ "v4l2loopback" ];

  hardware.asahi = {
    use4KPages = false;
    extractPeripheralFirmware = false;
    addEdgeKernelConfig = true;
  };

  nixpkgs.overlays = [
    (import ../../overlays/50-haskell.nix)
    (import ../../overlays/50-home-manager.nix)
    (import ../../overlays/50-vim-plugins.nix)
    (import ../../overlays/50-st)
    (import ../../overlays/50-intellij.nix)
  ];

  system.stateVersion = "22.11";

  # nixpkgs.config.allowUnfree = true;

  time.timeZone = "Europe/Paris";

  hardware.opengl = {
    enable = true;
  };
}
