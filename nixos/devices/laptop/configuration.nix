{ config, pkgs, lib, ... }:

let

  wifi = "wlp3s0";

in

{
  imports =
    [
      ./hardware-configuration.nix
      ./hardware-configuration-custom.nix
      ../../modules/wireless.nix
      ../../modules/xserver.nix
      # ../../modules/prometheus.nix
      # ../../modules/grafana.nix
      ../../modules/wlroots-screen-share.nix
      ../../modules/podman.nix
    ];

  me.wifi-interface = wifi;

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
    # kernelPackages = pkgs.linuxPackages_latest;
    tmp.cleanOnBoot = true;

    binfmt.emulatedSystems = [ "aarch64-linux" ];

  };


  time.timeZone = "Europe/Paris";

  environment = {
    systemPackages = with pkgs; [
      vim
      curl
      file
      tmux
      htop
      git
      usbutils
      pciutils
      unzip
      zip
      moreutils
      man-pages
      gnome3.adwaita-icon-theme
    ];

    etc."resolv.conf".text = "nameserver 8.8.8.8";
    etc."jdk".source = pkgs.jdk;
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
      extraPackages = with pkgs; [ xwayland swayidle swaylock i3status grim slurp wayvnc ];
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
      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm"
        # "ogfcmafjalglgifnmanfmnieipoejdcf"
      ];
      extraOpts = {
        RestoreOnStartup = 1;
        HttpsOnlyMode = "force_enabled";
        BrowserSignin = 0;
        SyncDisabled = true;
        PasswordManagerEnabled = false;
        # BuiltInDnsClientEnabled = false;
        MetricsReportingEnabled = false;
        SpellcheckEnabled = true;
        SpellcheckLanguage = [ "en-US" ];
        CloudPrintSubmitEnabled = false;
      };
    };
  };

  networking = {
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 8000 ];
    nameservers = [ "8.8.8.8" ];
    hostName = "laptop";
    useNetworkd = true;
    dhcpcd.enable = false;
    useDHCP = false;
    extraHosts = config.me.private.networking.extraHosts;
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
      # enable = false;
    };
  };

  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  services = {
    xserver.enable = false;

    tlp = {
      enable = true;
      settings = {
        START_CHARGE_THRESH_BAT0 = 91;
        STOP_CHARGE_THRESH_BAT0 = 96;
        CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
        CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";
        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
      };
    };

    fstrim.enable = true;

    postgresql = {
      enable = false;
      extraPlugins = [ pkgs.postgis ];
    };

    resolved = {
      enable = false;
    };

    upower = {
      enable = false;
    };
  };

  services.nscd.enable = false;
  system.nssModules = lib.mkForce [];

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
        jetbrains.idea-community-minimal
        vscode
        scala-cli
      ];
    };

    users.dev2 = {
      isNormalUser = true;
      uid = 1003;
      packages = with pkgs; [
        vscode
      ];
    };
  };

  nix = {
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      experimental-features = nix-command flakes
    '';
    settings.sandbox = true;
  };

  zramSwap = {
    enable = false;
    algorithm = "zstd";
    memoryPercent = 40;
  };

  documentation.dev.enable = true;

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "20.03";

}
