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

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.gfxmodeBios= "text";
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.loader.grub.extraEntries = ''
    menuentry "Arch" {
      configfile /grub.arch4/grub.cfg
    } 
  '';
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
  boot.kernelPackages = (import <nixos-unstable> {}).linuxPackages_4_16;

  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  time.timeZone = "Europe/Paris";

  environment.systemPackages = with pkgs; [
    vim
    curl
    ripgrep
    ag
    file
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
    pavucontrol
    (pkgs.vim_configurable.customize {
      name = "vimx";
      vimrcConfig.packages.myplugins = {
        start = [ pkgs.vimPlugins.vim-nix ];
      };
    })
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = false; };
  programs.ssh.startAgent = true;

  # services.openssh.enable = true;

  networking = {

    firewall.allowedTCPPorts = [ ];
    # firewall.allowedUDPPorts = [ ... ];
    firewall.enable = true;
    nameservers = [ "8.8.8.8" ];
    hostName = "laptop";

  };

  # services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver.enable = false;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  services.tlp.enable = true;
  services.fstrim.enable = true;

  users.users.me = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "systemd-journal" ];
    group = "me";
  };

  users.groups.me = {
    gid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

  security.hideProcessInformation = true;

}

