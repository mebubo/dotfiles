{ config, lib, pkgs, modulesPath, ... }:

{
  boot.initrd.luks.devices."root" = {
    device = "/dev/disk/by-label/nixos3";
    allowDiscards = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos3-root";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/EFI";
    fsType = "vfat";
  };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  hardware.enableAllFirmware = lib.mkForce false;
  hardware.enableRedistributableFirmware = lib.mkForce false;

  boot.initrd.kernelModules = [ "apple_dcp" ];

  boot.initrd.postDeviceCommands = ''
      echo 0 > /sys/class/backlight/apple-panel-bl/brightness
      sleep 1
      echo 420 > /sys/class/backlight/apple-panel-bl/brightness
  '';

  system.activationScripts = {
    brightness = ''
      echo 0 > /sys/class/backlight/apple-panel-bl/brightness
      sleep 1
      echo 420 > /sys/class/backlight/apple-panel-bl/brightness
    '';
  };
}
