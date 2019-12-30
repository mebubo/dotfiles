{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./hardware-configuration-custom.nix
      ./wireless.nix
      ../../modules/xserver.nix
    ];

  boot = {
    loader.grub = {
      enable = true;

      # BIOS
      # device = "/dev/sda";

      # UEFI
      device = "nodev";
      efiSupport = true;
      efiInstallAsRemovable = true;

      gfxmodeBios= "text";
      splashImage = null;
      font = null;
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
    ];

    etc."resolv.conf".text = "nameserver 8.8.8.8";
    etc."jdk".source = pkgs.jdk;
    etc."jdk12".source = pkgs.jdk12;
  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    cascadia-code
  ];

  programs = {
    gnupg.agent = { enable = true; enableSSHSupport = false; };
    ssh.startAgent = true;
    java.enable = true;
    sway = {
      enable = true;
      extraPackages = with pkgs; [ xwayland swayidle swaylock i3status grim slurp ];
      extraSessionCommands = ''
        export _JAVA_AWT_WM_NONREPARENTING=1
        export MOZ_ENABLE_WAYLAND=1
        export MOZ_USE_XINPUT2=1
      '';
    };
    dconf.enable = false;
    less = {
      enable = true;
      envVariables = {
        LESS = "--quit-if-one-screen --jump-target=5";
      };
    };
    chromium = {
      enable = true;
      extensions = [ "cjpalhdlnbpafiamejdnhcphjbkeiagm" ];
      extraOpts = {
        RestoreOnStartup = 1;
      };
    };
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
         "50-ethernet" = {
           enable = true;
           matchConfig = { Name = "enp*s*"; };
           networkConfig = { DHCP = "ipv4"; };
           dhcpConfig = { RouteMetric = 300; };
         };
         "50-wireless" = {
           enable = true;
           matchConfig = { Name = "wlp*s*"; };
           networkConfig = { DHCP = "ipv4"; };
           dhcpConfig = { RouteMetric = 1000; UseDNS = false; };
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

    pipewire = {
      enable = false;
    };

    upower = {
      enable = false;
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

    users.dev = {
      isNormalUser = true;
      uid = 1002;
      packages = with pkgs; [
        alacritty
        cage
        firefox-wayland
        jetbrains.idea-community
        sbt
        x11docker
        xpra
      ];
    };

    users.dev2 = {
      isNormalUser = true;
      uid = 1003;
      packages = with pkgs; [
        alacritty
        firefox
        google-chrome
        nodejs
        vscode
        i3
        i3lock
        i3status
      ];
    };

    groups.wireless = {};
  };

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

  documentation.dev.enable = true;

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "20.03";

}
