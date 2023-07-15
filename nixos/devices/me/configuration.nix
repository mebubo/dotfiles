{ dotfiles-private }:
{ config, pkgs, ... }:

let

  nixos-m1 = builtins.fetchTarball {
    url = "https://github.com/tpwrules/nixos-apple-silicon/archive/ef6f0de57ef175e0de8c7e846a95481ac6f4ce58.tar.gz";
    sha256 = "0sfmwlmnm3ga0g4kfz3zml41v16n4a2sxif49mhix46al9indl49";
  };

  m1-support = nixos-m1 + "/apple-silicon-support";

  wifi = "wlp1s0f0";

in

{
  imports =
    [
      ./hardware-configuration.nix
      (import ../../modules/wireless.nix { inherit dotfiles-private; interface = wifi; })
      m1-support
      ../../modules/wlroots-screen-share.nix
    ];

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
      tar xf ${./firmware.tar} -C $out/lib/firmware
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

  # nixpkgs.config.allowUnfree = true;

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
