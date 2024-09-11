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

  users.users.me = {
    isNormalUser = true;
    extraGroups = [ "systemd-journal" "libvirtd" "wireless" ];
    packages = with pkgs; [
      google-chrome
    ];
  };

  users.users.dev = {
    isNormalUser = true;
    # group = "users";
    openssh.authorizedKeys.keyFiles = [ config.me.private.keys.me-fr ];
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

  system.stateVersion = "22.11";

  nixpkgs.config = {
    allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "obsidian"
      "vscode"
      "chromium"
      "chromium-unwrapped"
      "widevine-cdm"
      "google-chrome"
    ];
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = false;
    };
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
      curl
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

  services.logind = {
    lidSwitch = "suspend";
    lidSwitchDocked = "suspend";
    lidSwitchExternalPower = "suspend";
    powerKey = "suspend";
    powerKeyLongPress = "poweroff";
  };

  nixpkgs.overlays = [
    (self: super: {
      chromium = super.chromium.override { enableWideVine = true; };
    })
  ];
}
