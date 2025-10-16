{ config, pkgs, lib, ... }:

let

  # wifi = "wlp1s0";
  wifi = "wlp166s0";

  chrome-version = "131.0.6778.204";
  chrome-hash = "sha256-vAZUFufRfvkRsbXnqWD4zE3hgTWbhFqDlauXN7m6mIw=";
  google-chrome-overlay = self: super: super.google-chrome.overrideAttrs (oldAttrs: {
    version = chrome-version;
    src = pkgs.fetchurl {
      url = "https://dl.google.com/linux/chrome/deb/pool/main/g/google-chrome-stable/google-chrome-stable_${chrome-version}-1_amd64.deb";
      hash = chrome-hash;
    };
  });

  nixos-rebuild-fr = pkgs.writeShellScriptBin "nixos-rebuild-fr" ''
    [[ "$PWD" != "/root" ]] && exit 1
    ACTION=''${1:-boot}
    rm -fr dotfiles/
    cp -r /home/me/src/me/dotfiles .
    nixos-rebuild $ACTION --flake /root/dotfiles#fr --print-build-logs --log-format bar-with-logs
  '';

  chromium-customized-for-overlay = p: p.chromium.override {
    enableWideVine = true;
    commandLineArgs = [
      "--password-store=gnome-libsecret"
    ];
  };

  libsecretPath = pkgs.lib.makeLibraryPath [ pkgs.libsecret ];

  chromium-with-libsecret = pkgs.writeShellScriptBin "c" ''
    export LD_LIBRARY_PATH=${libsecretPath}:$LD_LIBRARY_PATH
    exec ${pkgs.chromium}/bin/chromium --user-data-dir="$HOME/chromium-10" "$@"
  '';


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
      ../../modules/podman.nix
      ../../modules/laptop-ap.nix
    ];

  i18n = {
    defaultLocale = "fr_FR.UTF-8";
    extraLocales = [ "en_US.UTF-8/UTF-8" ];
  };

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

  users.users.game = {
    isNormalUser = true;
    uid = 1003;
    openssh.authorizedKeys.keyFiles = [ config.me.private.keys.me-fr ];
  };

  users.users.scr = {
    isNormalUser = true;
    uid = 1004;
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
      "tokenizer.json"
      "claude-code"
      "windsurf"
      "steam"
      "steam-original"
      "steam-unwrapped"
      "steam-run"
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
        rofi
        grim
        i3status
        i3status-rust
        playerctl
        slurp
        swayidle
        swaylock
        # wayvnc
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
        # UBO
        # "cjpalhdlnbpafiamejdnhcphjbkeiagm"
        # UBOL
        "ddkjiahejlhfcafbddmgiahcphecmpfh"
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
    seahorse = {
      enable = true;
    };
    bandwhich = {
      enable = true;
    };
    captive-browser = {
      enable = true;
      interface = wifi;
      dhcp-dns = "${pkgs.systemd}/bin/networkctl status ${wifi} | ${pkgs.gnugrep}/bin/grep -oP 'DNS: \\\\K[0-9.]+'";
    };
    nix-ld = {
      enable = true;
    };
    steam = {
      enable = true;
    };
    hyprland = {
      enable = true;
      withUWSM = true;
    };
    hyprlock = {
      enable = true;
    };
    waybar = {
      enable = true;
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
    extraPortals = [ pkgs.xdg-desktop-portal-hyprland ];
  };

  environment = {
    systemPackages = with pkgs; [
      vim
      git
      curl
      hdparm
      nixos-rebuild-fr
      chromium-with-libsecret
    ];

    # etc."resolv.conf".text = ''
    #   nameserver 8.8.8.8
    #   nameserver 1.1.1.1
    # '';
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
    nerd-fonts.caskaydia-mono
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
        OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
        WEBUI_AUTH = "False";
      };
    };
    # private-gpt = {
    #   enable = true;
    # };
  };

  services.power-profiles-daemon.enable = true;

  services.logind.settings.Login = {
    HandleLidSwitch = "suspend";
    HandleLidSwitchDocked = "suspend";
    HandleLidSwitchExternalPower = "suspend";
    HandlePowerKey = "hibernate";
    HandlePowerKeyLongPress = "poweroff";
  };

  services.zimply = {
    enable = true;
    zimPath = "/home/zim";
  };

  services.gnome.gnome-keyring.enable = true;
  services.gnome.gcr-ssh-agent.enable = false;
  security.pam.services.login.enableGnomeKeyring = true;

  security.polkit = {
    enable = true;
  };

  nixpkgs.overlays = [
    (self: super: {
      chromium = chromium-customized-for-overlay super;
      # google-chrome = google-chrome-overlay self super;
    })
  ];
}
