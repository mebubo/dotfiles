{ pkgs, lib, modulesPath, config, ... }:

{

  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
    ./dev.nix
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    autoResize = true;
    fsType = "ext4";
  };

  boot.growPartition = true;
  boot.kernelParams = ["console=ttyS0"];
  boot.loader.grub.device = "/dev/vda";

  boot.loader.grub.efiSupport = false;
  boot.loader.timeout = 0;

  system.build.qcow = import "${toString modulesPath}/../lib/make-disk-image.nix" {
    inherit lib config pkgs;
    diskSize = 50000;
    format = "qcow2";
    partitionTableType = "hybrid";
  };

  networking = {
    useDHCP = false;
    usePredictableInterfaceNames = false;
    interfaces.eth0.useDHCP = true;
    nameservers = [ "8.8.8.8" ];
    firewall.enable = false;
  };

  users.users.root = {
    openssh.authorizedKeys.keys = map builtins.readFile config.me.private.keys;
  };

  users.users.me = {
    isNormalUser = true;
    uid = 1000;
    password = "me";
    openssh.authorizedKeys.keys = map builtins.readFile config.me.private.keys;
    packages = (with pkgs; [
      htop
      git
      tmux
      nodejs
      python3
      python3Packages.pip
      cmake
    ]);
  };

  services = {
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
      };
    };
  };

  documentation.enable = false;

  system.stateVersion = "23.05";

}
