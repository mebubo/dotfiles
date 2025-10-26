{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/5a45fc57-2b26-4fa2-ac37-937cb1a9107f";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."root" = {
    device = "/dev/disk/by-uuid/fdc5f572-23f4-4c37-8d78-6d2c4a841608";
    allowDiscards = true;
  };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/7ECF-3D5E";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault false;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # ---

  systemd.network = {
    enable = true;
    networks."10-wireless" = {
      matchConfig.Name = "wlp*";
      networkConfig.DHCP = "yes";
      dhcpV4Config.RouteMetric = 600;
    };
    networks."20-ethernet" = {
      matchConfig.Name = "enp*";
      networkConfig.DHCP = "yes";
      dhcpV4Config.RouteMetric = 100;
    };
  };
  services.resolved.enable = true;

  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  hardware.sensor.iio.enable = true;
}
