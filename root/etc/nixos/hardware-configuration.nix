{ config, lib, pkgs, ... }:

let

  external-display-hotplug = pkgs.writeScript "external-display-hotplug" ''
#!${pkgs.stdenv.shell}

STATUS_FILE=/sys/class/drm/card0-DP-1/status
STATUS=$(${pkgs.coreutils}/bin/cat $STATUS_FILE)
export SWAYSOCK=$(${pkgs.coreutils}/bin/readlink -f /run/user/1000/sway-ipc.1000.*.sock)

env >> /tmp/h

case $STATUS in
  disconnected)
    ${pkgs.sway}/bin/swaymsg output LVDS-1 enable
    ${pkgs.sway}/bin/swaymsg output DP-1 disable
    ;;
  connected)
    ${pkgs.sway}/bin/swaymsg output DP-1 enable
    ${pkgs.sway}/bin/swaymsg output LVDS-1 disable
    ;;
  esac
'';

in
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
      options = [ "subvol=@nixos,compress=lzo" ];
    };

  fileSystems."/home" =
    { device = "/dev/mapper/root";
      fsType = "btrfs";
      options = [ "subvol=@home,compress=lzo" ];
    };

  fileSystems."/boot" =
    { device = "/dev/sda1";
      fsType = "ext4";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  hardware.cpu.intel.updateMicrocode = true;

  services.udev.extraRules = ''
    SUBSYSTEM=="rfkill", ATTR{type}=="bluetooth", ATTR{state}="0"
    ACTION=="change", SUBSYSTEM=="drm", ENV{HOTPLUG}=="1", RUN+="${pkgs.sudo}/bin/sudo -u me ${external-display-hotplug}"
  '';

}
