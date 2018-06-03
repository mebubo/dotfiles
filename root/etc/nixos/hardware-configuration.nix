{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
  boot.kernelParams = [ ''acpi_osi="!Windows 2012"'' ];

  boot.initrd.luks.devices."root" = {
    device = "/dev/sda3";
    allowDiscards = true;
  };

  fileSystems."/" =
    { device = "/dev/mapper/root";
      fsType = "btrfs";
      options = [ "subvol=@nixos2,compress=lzo" ];
    };

  fileSystems."/home" =
    { device = "/dev/mapper/root";
      fsType = "btrfs";
      options = [ "subvol=@home4,compress=lzo" ];
    };

  fileSystems."/boot" =
    { device = "/dev/sda2";
      fsType = "ext2";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware.cpu.intel.updateMicrocode = true;

  services.udev.extraRules = ''
    SUBSYSTEM=="rfkill", ATTR{type}=="bluetooth", ATTR{state}="0"
    ACTION=="change", SUBSYSTEM=="drm", ENV{HOTPLUG}=="1", RUN+="${pkgs.sudo}/bin/sudo -u me DISPLAY=:0 /usr/local/bin/external-display.sh"
  '';

}
