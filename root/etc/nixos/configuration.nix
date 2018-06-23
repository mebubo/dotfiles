# See:
# - configuration.nix(5)
# - NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./wireless.nix
      ./xserver.nix
    ];

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      gfxmodeBios= "text";
      device = "/dev/sda";
      extraEntries = ''
        menuentry "Arch" {
          configfile /grub.arch4/grub.cfg
        }
      '';
    };
    kernel.sysctl."fs.inotify.max_user_watches" = 524288;
    kernelPackages = pkgs.linuxPackages_latest;
    cleanTmpDir = true;
  };


  time.timeZone = "Europe/Paris";

  environment.systemPackages = with pkgs; [
    vim
    curl
    ripgrep
    ag
    file
    tree
    tmux
    htop
    git
    i3
    i3status
    i3lock
    xss-lock
    dmenu
    st
    firefox
    chromium
    pavucontrol
    (pkgs.vim_configurable.customize {
      name = "vimx";
      vimrcConfig.packages.myplugins = {
        start = [ pkgs.vimPlugins.vim-nix ];
      };
    })
    jdk
    sbt
    stack
    cabal2nix
    cabal-install
    haskellPackages.ghc
    haskellPackages.ghcid
    mpv
  ];

  programs = {
    gnupg.agent = { enable = true; enableSSHSupport = false; };
    ssh.startAgent = true;
    java.enable = true;
  };

  networking = {
    firewall.enable = true;
    nameservers = [ "8.8.8.8" ];
    hostName = "laptop";
  };

  sound.enable = true;

  hardware.pulseaudio.enable = true;

  services = {
    xserver.enable = false;

    tlp = {
      enable = true;
      extraConfig = ''
        START_CHARGE_THRESH_BAT0=91
        STOP_CHARGE_THRESH_BAT0=96
      '';
    };


    fstrim.enable = true;

    nscd.enable = false;

    postgresql = {
      enable = true;
      extraPlugins = [ pkgs.postgis ];
    };
  };

  users = {
    users.me = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "systemd-journal" "libvirtd" ];
      group = "me";
    };

    groups.me = {
      gid = 1000;
    };
  };

  system.nixos.stateVersion = "18.03";

  security.hideProcessInformation = true;

}
