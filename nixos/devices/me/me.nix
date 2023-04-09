{ config, pkgs, lib, modulesPath, ... }:

let

  # latest pre-jcef commit, works on aarch64-linux
  # https://hydra.nixos.org/job/nixos/trunk-combined/nixpkgs.jetbrains.jdk.aarch64-linux
  pkgs-jetbrains-jdk = import (pkgs.fetchzip {
    url = "https://github.com/NixOS/nixpkgs/archive/06278c77b5d162e62df170fec307e83f1812d94b.tar.gz";
    sha256 = "1javsbaxf04fjygyp5b9c9hb9dkh5gb4m4h9gf9gvqlanlnms4n5";
  }) {};

  jdk = pkgs-jetbrains-jdk.callPackage ../../packages/jetbrains/jetbrains-jdk.nix {};
  intellij = pkgs.callPackage ../../packages/jetbrains/intellij.nix { inherit jdk; };

in

{

  users = {
    users = {

      me = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "systemd-journal" "libvirtd" ];
        group = "me";
      };

      dev = {
        isNormalUser = true;
        uid = 1002;
        packages = with pkgs; [
          intellij
        ];
      };

      dev2 = {
        isNormalUser = true;
        uid = 1003;
        packages = with pkgs; [
        ];
      };

    };

    groups.me = {
      gid = 1000;
    };

    groups.wireless = {};
  };

  networking.wireless = {
    enable = true;
    interfaces = [ "wlp1s0f0" ];
    networks = (import ./private.nix).wireless.networks;
    userControlled = {
      enable = true;
      group = "wireless";
    };
  };

  networking = {
    hostName = "me";
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 8000 ];
    useDHCP = true;
    interfaces.wlp1s0f0.useDHCP = true;
  };

  programs = {
    gnupg.agent = { enable = true; enableSSHSupport = false; };
    ssh.startAgent = true;
    java.enable = true;
    sway = {
      enable = true;
      extraPackages = with pkgs; [ xwayland swayidle swaylock i3status grim slurp wayvnc gnome.adwaita-icon-theme brightnessctl ];
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

  console.packages = [ pkgs.terminus_font ];

  environment = {
    systemPackages = [
      pkgs.gptfdisk
      pkgs.parted
      pkgs.cryptsetup
      pkgs.vim
      pkgs.git
      pkgs.tmux
      pkgs.htop
    ];
    etc."jdk".source = pkgs.openjdk;
  };

  sound.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

}
