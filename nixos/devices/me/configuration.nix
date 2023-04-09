{ config, pkgs, ... }:

let

  nixos-m1 = builtins.fetchTarball {
    url = "https://github.com/tpwrules/nixos-apple-silicon/archive/696d7b4ac7ca05684a3cceedd44acfd0d1c5f1e3.tar.gz";
    sha256 = "0cj2xsnnvxffkzjqla3i6inpyrgfqws6gk79p1kllnd3aw3dckay";
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
    useExperimentalGPUDriver = false;
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

  fonts.fontconfig = {
    enable = true;
    antialias = true;
    hinting = {
      enable = true;
    };
  };
}
