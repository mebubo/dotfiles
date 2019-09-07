# See:
# - configuration.nix(5)
# - NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./wireless.nix
    ];

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      gfxmodeBios= "text";
      device = "/dev/sda";
      splashImage = null;
    };
    kernel.sysctl = {
      "fs.inotify.max_user_watches" = 524288;
      "kernel.sysrq" = 1;
    };
    kernelPackages = pkgs.linuxPackages_latest;
    cleanTmpDir = true;
  };

  time.timeZone = "Europe/Paris";

  environment = {
    systemPackages = with pkgs; [
      vim
      curl
      wget
      ripgrep
      ag
      file
      tree
      tmux
      htop
      git
      usbutils
      pciutils
      unzip
      zip
      moreutils
      jq
      ctags
      pavucontrol
      mpv
      i3status
      dmenu
      manpages
      ddccontrol
    ];

    etc."resolv.conf".text = "nameserver 8.8.8.8";
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-emoji
  ];

  programs = {
    gnupg.agent = { enable = true; enableSSHSupport = false; };
    ssh.startAgent = true;
    java.enable = true;
    sway = {
      enable = true;
      extraPackages = with pkgs; [ xwayland swayidle swaylock i3status grim slurp bemenu ];
      extraSessionCommands = ''
        export _JAVA_AWT_WM_NONREPARENTING=1
        export MOZ_ENABLE_WAYLAND=1
        export MOZ_USE_XINPUT2=1
      '';
    };
    dconf.enable = false;
  };

  networking = {
    firewall.enable = true;
    # firewall.allowedTCPPorts = [ 8000 ];
    nameservers = [ "8.8.8.8" ];
    hostName = "laptop";
    useNetworkd = true;
    dhcpcd.enable = false;
    useDHCP = false;
  };

  systemd = {
    network = {
      enable = true;
      networks = {
         "50-lo" = {
           enable = true;
           matchConfig = { Name = "lo"; };
         };
         "50-ethernet" = {
           enable = true;
           matchConfig = { Name = "enp*s*"; };
           networkConfig = { DHCP = "v4"; };
           dhcpConfig = { RouteMetric = 300; };
         };
         "50-wireless" = {
           enable = true;
           matchConfig = { Name = "wlp*s*"; };
           networkConfig = { DHCP = "v4"; };
           dhcpConfig = { RouteMetric = 1000; UseDNS = false; };
         };
         "99-main" = {
           enable = false;
         };
      };
    };

    services.systemd-networkd-wait-online = {
      enable = false;
    };
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
      enable = false;
      extraPlugins = [ pkgs.postgis ];
    };

    resolved = {
      enable = false;
    };

    bloop = {
      install = true;
    };

    pipewire = {
      enable = true;
    };
  };

  users = {
    users.me = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "systemd-journal" "libvirtd" ];
      group = "me";
    };

    users.dev = {
      isNormalUser = true;
      uid = 1002;
    };

    groups.me = {
      gid = 1000;
    };

    groups.wireless = {};
  };

  system.stateVersion = "19.03";

  security.hideProcessInformation = false;

  nix = {
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';
    useSandbox = true;
  };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 40;
  };

  nixpkgs.config = {
    packageOverrides = super: {
      bemenu = super.bemenu.override {
        waylandSupport = true;
        ncursesSupport = false;
        x11Support = false;
      };
    };
  };

  documentation.dev.enable = true;
}
