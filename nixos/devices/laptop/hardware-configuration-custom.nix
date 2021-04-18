{ config, lib, pkgs, ... }:

{

  boot.initrd.availableKernelModules = [ "i915" ];

  # boot.kernelParams = [ ''acpi_osi="!Windows 2012"'' "i915.enable_rc6=7" ];
  boot.kernelParams = [ ''acpi_osi="!Windows 2012"'' ];

  boot.initrd.luks.devices."root" = {
    allowDiscards = true;
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="rfkill", ATTR{type}=="bluetooth", ATTR{state}="0"
  '';

  hardware.cpu.intel.updateMicrocode = true;

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
    ];
  };

  security.allowSimultaneousMultithreading = false;
}
