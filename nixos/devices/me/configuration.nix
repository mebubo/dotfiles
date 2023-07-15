{ config, pkgs, ... }:

let

  wifi = "wlp1s0f0";

in

{
  imports =
    [
      ./hardware-configuration.nix
      ./me.nix
      ../../modules/wireless.nix
      ../../modules/wlroots-screen-share.nix
    ];

  me.wifi-interface = wifi;

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "0";
  boot.loader.efi.canTouchEfiVariables = false;

  hardware.asahi = {
    use4KPages = false;
    extractPeripheralFirmware = false;
    addEdgeKernelConfig = true;
    useExperimentalGPUDriver = false;
  };

  hardware.firmware = [
    (pkgs.runCommand "firmware-extract" {} ''
      mkdir -p $out/lib/firmware
      tar xf ${dotfiles-private.firmware} -C $out/lib/firmware
    '')
  ];

  networking = {
    hostName = "me";
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 8000 ];
    useDHCP = true;
    interfaces.${wifi}.useDHCP = true;
  };

  system.stateVersion = "22.11";

  nixpkgs.config.allowUnfree = true;

  time.timeZone = "Europe/Paris";

  hardware.opengl = {
    enable = true;
  };

  fonts.fontconfig = {
    enable = true;
    antialias = true;
    hinting = {
      enable = true;
    };
  };
}
