{ pkgs, lib, config, ... }:

{

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;
  # boot.consoleLogLevel = lib.mkDefault 7;
  boot.kernelParams = ["console=ttyS0,115200n8" "console=ttyAMA0,115200n8" "console=tty0"];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.kernelModules = [ "vc4" "bcm2835_dma" "i2c_bcm2835" ];
  boot.supportedFilesystems = lib.mkForce [ "vfat" "f2fs" "ext4" ];

  hardware.enableRedistributableFirmware = true;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    vim
    tmux
    htop
    moreutils
    git
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  boot.tmp.cleanOnBoot = true;

  documentation.nixos.enable = false;

  system.stateVersion = "23.05";

}
