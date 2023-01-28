{ config, lib, pkgs, modulesPath, ... }:

{

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/EFI";
    fsType = "vfat";
  };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  hardware.video.hidpi.enable = lib.mkDefault true;

  hardware.enableAllFirmware = lib.mkForce false;
  hardware.enableRedistributableFirmware = lib.mkForce false;

  system.activationScripts = {
    brightness = ''
      echo 0 > /sys/class/backlight/apple-panel-bl/brightness
      sleep 1
      echo 420 > /sys/class/backlight/apple-panel-bl/brightness
    '';
  };
}
