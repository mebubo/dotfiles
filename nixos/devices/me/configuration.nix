{ config, pkgs, ... }:

let

  nixos-m1 = builtins.fetchTarball {
    url = "https://github.com/tpwrules/nixos-apple-silicon/archive/0086dd0e401f9dde826e6c170d312a87564aa6a4.tar.gz";
    sha256 = "13krn3b54w5rfrqyn5czwddh2jhggdx5wkysj1kz7n75mv3z1frv";
  };

  m1-support = nixos-m1 + "/apple-silicon-support";

in

{
  imports =
    [
      ./hardware-configuration.nix
      ./me.nix
      m1-support
      ./home-manager.nix
      ../../modules/wlroots-screen-share.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.consoleMode = "0";
  boot.loader.efi.canTouchEfiVariables = false;

  hardware.asahi = {
    use4KPages = false;
    extractPeripheralFirmware = false;
    addEdgeKernelConfig = true;
  };

  hardware.firmware = [
    (pkgs.runCommand "firmware-extract" {} ''
      mkdir -p $out/lib/firmware
      tar xf ${./firmware.tar} -C $out/lib/firmware
    '')
  ];

  nixpkgs.overlays = [
    (import ../../overlays/50-haskell.nix)
    (import ../../overlays/50-home-manager.nix)
    (import ../../overlays/50-vim-plugins.nix)
    (import ../../overlays/50-st)
    (import ../../overlays/50-intellij.nix)
  ];

  system.stateVersion = "22.11";

  # nixpkgs.config.allowUnfree = true;

  time.timeZone = "Europe/Paris";

  hardware.opengl = {
    enable = true;
  };
}
