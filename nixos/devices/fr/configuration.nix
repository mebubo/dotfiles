{ config, pkgs, lib, ... }:

let

  wifi = "wlp1s0";

in

{
  imports =
    [
      ./hardware-configuration.nix
      # ../../modules/framework.nix
      ../../modules/wireless.nix
      # ../../modules/wlroots-screen-share.nix
      # ../../modules/prometheus.nix
      # ../../modules/grafana.nix
    ];

  me.wifi-interface = wifi;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  networking.hostName = "fr";
  networking.extraHosts = config.me.private.networking.extraHosts;

  time.timeZone = "Europe/Paris";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.me = {
    isNormalUser = true;
    extraGroups = [ "systemd-journal" "libvirtd" "wireless" ];
    packages = with pkgs; [
      # git
      # tmux
      # vim
    ];
  };

  users.users.dev = {
    isNormalUser = true;
    # group = "users";
    openssh.authorizedKeys.keyFiles = config.me.private.keys;
    packages = with pkgs; [
      cmake
      ninja
      gnumake
      pkg-config
      gcc
    ];
  };

  # users.groups.dev = {};

  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
    };
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 8000 8090 ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  nixpkgs.config = {
    allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "obsidian"
      "vscode"
      "chromium"
      "chromium-unwrapped"
      "widevine-cdm"
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  programs = {
    gnupg.agent = { enable = true; enableSSHSupport = false; };
    ssh.startAgent = true;
    sway = {
      enable = true;
      extraPackages = with pkgs; [ xwayland swayidle swaylock i3status i3status-rust wev grim slurp wayvnc brightnessctl wl-clipboard playerctl ];
      extraSessionCommands = ''
        export _JAVA_AWT_WM_NONREPARENTING=1
        export MOZ_ENABLE_WAYLAND=1
        export MOZ_USE_XINPUT2=1
      '';
      wrapperFeatures = {
        base = true;
        gtk = true;
      };
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
    bandwhich = {
      enable = true;
    };
    captive-browser = {
      enable = true;
      interface = wifi;
    };
  };

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  nix = {
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      experimental-features = nix-command flakes
    '';
    settings.sandbox = true;
  };

  services = {
    xserver.enable = false;
    fstrim.enable = true;
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  environment = {
    systemPackages = with pkgs; [
      vim
      git
      # curl
      # file
      # tmux
      # htop
    ];

    etc."resolv.conf".text = ''
      nameserver 8.8.8.8
      nameserver 1.1.1.1
    '';
  };

  fonts.packages = with pkgs; [
    monaspace
    font-awesome
  ];

  services.power-profiles-daemon.enable = true;

  nixpkgs.overlays = [
    (self: super: {
      chromium = super.chromium.override { enableWideVine = true; };
    })
  ];
}
