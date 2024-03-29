{ config, lib, pkgs, ... }:

{

  boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.initrd.luks.devices."root" = {
    device = "/dev/sda3";
    allowDiscards = true;
  };

  fileSystems."/" = {
    device = "/dev/mapper/root";
    fsType = "btrfs";
    options = [ "subvol=@nixos" "compress=lzo" ];
  };

  fileSystems."/home" = {
    device = "/dev/mapper/root";
    fsType = "btrfs";
    options = [ "subvol=@home" "compress=lzo" ];
  };

  fileSystems."/boot" = {
    device = "/dev/sda1";
    fsType = "vfat";
  };

  swapDevices = [ ];

  nix.settings.max-jobs = lib.mkDefault 4;
}
