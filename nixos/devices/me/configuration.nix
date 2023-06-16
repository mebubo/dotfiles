{ dotfiles-private }:
{ config, pkgs, ... }:

let

  nixos-m1 = builtins.fetchTarball {
    url = "https://github.com/tpwrules/nixos-apple-silicon/archive/002d7e27b1de5885b14c2b2ed3be13c9ad862d23.tar.gz";
    sha256 = "1k4y51b16yy8g4imdd2z4fn7y1lz1pzj91kqjpl809nf8j42fqvy";
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
