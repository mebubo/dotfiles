{ config, pkgs, lib, ... }:

let

  wifi = "wlp1s0";
  chrome-version = "130.0.6723.91";
  chrome-hash = "sha256-30RwtLlU4GhqDhbHTZMqrih77d2yOFeIBiOG3CugvLo=";

in

{
  imports =
    [
      ./hardware-configuration.nix
      # ../../modules/framework.nix
      ../../modules/wireless.nix
      # ../../modules/wlroots-screen-share.nix
      ../../modules/prometheus.nix
      ../../modules/grafana.nix
    ];

  me.wifi-interface = wifi;

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.memtest86.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  networking.hostName = "fr";
  networking.extraHosts = config.me.private.networking.extraHosts;

  time.timeZone = "Europe/Paris";

  users.users.me = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "systemd-journal" "libvirtd" "wireless" "dialout" ];
  };

  users.users.dev = {
    isNormalUser = true;
    uid = 1001;
    openssh.authorizedKeys.keyFiles = [ config.me.private.keys.me-fr ];
  };

  users.users.doc = {
    isNormalUser = true;
    uid = 1002;
    openssh.authorizedKeys.keyFiles = [ config.me.private.keys.me-fr ];
  };

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
      "cursor"
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
      extraPackages = with pkgs; [
        brightnessctl
        rofi-wayland
        grim
        i3status
        i3status-rust
        playerctl
        slurp
        swayidle
        swaylock
        wayvnc
        wev
        wl-clipboard
        wlr-randr
        xwayland
      ];
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
        BrowserSignin = 1;
        SyncDisabled = false;
        PasswordManagerEnabled = true;
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
      dhcp-dns = "${pkgs.systemd}/bin/networkctl status ${wifi} | ${pkgs.gnugrep}/bin/grep -oP 'DNS: \\\\K[0-9.]+'";
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
      hdparm
    ];

    etc."resolv.conf".text = ''
      nameserver 8.8.8.8
      nameserver 1.1.1.1
    '';
  };

  fonts.packages = with pkgs; [
    monaspace
    font-awesome
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    dejavu_fonts
    fira-code
    fira-code-symbols
  ];

  services = {
    ollama = {
      enable = true;
    };
    open-webui = {
      enable = false;
      environment = {
        ANONYMIZED_TELEMETRY = "False";
        DO_NOT_TRACK = "True";
        SCARF_NO_ANALYTICS = "True";
        # OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
        WEBUI_AUTH = "False";
      };
    };
    # private-gpt = {
    #   enable = true;
    # };
  };

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
      google-chrome = super.google-chrome.overrideAttrs (oldAttrs: {
        version = chrome-version;
        src = pkgs.fetchurl {
          url = "https://dl.google.com/linux/chrome/deb/pool/main/g/google-chrome-stable/google-chrome-stable_${chrome-version}-1_amd64.deb";
          hash = chrome-hash;
        };
      });
    })
  ];
}
