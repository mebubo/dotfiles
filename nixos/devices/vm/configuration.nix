{ pkgs, lib, modulesPath, ... }:

let

  ensure-home-me = pkgs.writeShellScript "ensure-home-me" ''
    mkdir -p /home/me
    chown me:users /home/me
  '';

  firefox = pkgs.firefox;

  chromium = pkgs.chromium;
  chromium-exe = "chromium";

  # chromium = pkgs.google-chrome;
  # chromium-exe = "google-chrome-stable";

  sway-config = pkgs.writeText "sway-config" ''
    set $mod Mod1

    bindsym $mod+d exec dmenu_path | dmenu | xargs swaymsg exec --
    bindsym $mod+Shift+d exec i3-dmenu-desktop
    bindsym $mod+Return exec ${pkgs.foot}/bin/foot
    bindsym $mod+Shift+c exec ${chromium}/bin/${chromium-exe}
    bindsym $mod+Shift+f exec ${firefox}/bin/firefox
    bindsym $mod+s layout stacking
    bindsym $mod+w layout tabbed
    bindsym $mod+e layout toggle split

    bindsym $mod+1 workspace 1
    bindsym $mod+2 workspace 2
    bindsym $mod+3 workspace 3
    bindsym $mod+4 workspace 4
    bindsym $mod+5 workspace 5
    bindsym $mod+6 workspace 6
    bindsym $mod+7 workspace 7
    bindsym $mod+8 workspace 8
    bindsym $mod+9 workspace 9
    bindsym $mod+0 workspace 10

    bindsym $mod+Shift+1 move container to workspace 1
    bindsym $mod+Shift+2 move container to workspace 2
    bindsym $mod+Shift+3 move container to workspace 3
    bindsym $mod+Shift+4 move container to workspace 4
    bindsym $mod+Shift+5 move container to workspace 5
    bindsym $mod+Shift+6 move container to workspace 6
    bindsym $mod+Shift+7 move container to workspace 7
    bindsym $mod+Shift+8 move container to workspace 8
    bindsym $mod+Shift+9 move container to workspace 9
    bindsym $mod+Shift+0 move container to workspace 10

    bindsym $mod+Shift+q reload
    bindsym $mod+Shift+e exec "swaynag -t warning -m 'Exit sway?' -b 'Yes' 'swaymsg exit'"

    bar {
            position bottom
            status_command ${pkgs.i3status}/bin/i3status
            font pango:monospace, FontAwesome 9
    }

    input type:keyboard xkb_layout us,ru
    input type:keyboard xkb_variant ,phonetic
    input type:keyboard xkb_options grp:rwin_toggle,grp_led:caps,ctrl:nocaps,compose:ralt

    output Virtual-1 mode 1600x1400

    output Virtual-1 scale 2

    # exec ${pkgs.foot}/bin/foot
  '';

  sway-run = pkgs.writeShellScriptBin "sway-run" ''
    # WLR_NO_HARDWARE_CURSORS=1 WLR_RENDERER_ALLOW_SOFTWARE=1 ${pkgs.sway}/bin/sway --config ${sway-config}
    WLR_NO_HARDWARE_CURSORS=1 WLR_RENDERER=pixman ${pkgs.sway}/bin/sway --config ${sway-config}
  '';

  i3-config = pkgs.writeText "i3-config" ''
    set $mod Mod1
    bindsym $mod+Return exec ${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal
    bindsym $mod+Shift+c exec ${chromium}/bin/${chromium-exe}
  '';

  i3-run = pkgs.writeShellScriptBin "i3-run" ''
    ${pkgs.xorg.xinit}/bin/startx ${pkgs.i3}/bin/i3 -c ${i3-config}
  '';

  cage-run = pkgs.writeShellScriptBin "cage-run" ''
    WLR_NO_HARDWARE_CURSORS=1 ${pkgs.cage}/bin/cage ${chromium}/bin/${chromium-exe}
  '';

in

{

  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
    ../../modules/home-manager.nix
    ../../modules/podman.nix
  ];

  config = {

    fileSystems."/" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=8G" "mode=755" ];
    };

    fileSystems."/home" = {
      device = "/dev/disk/by-label/home";
      fsType = "ext4";
    };

    fileSystems."/nix/store" = {
      device = "/dev/disk/by-label/nix-store";
      autoResize = true;
      fsType = "ext4";
    };

    boot.loader.grub.device = "/none";
    boot.kernelPackages = pkgs.linuxPackages_latest;

    networking = {
      useDHCP = false;
      useNetworkd = true;
      usePredictableInterfaceNames = false;
      interfaces.eth0.useDHCP = true;
      nameservers = [ "8.8.8.8" ];
      firewall.enable = false;
    };

    users.users.root = {
      openssh.authorizedKeys.keys = [ (builtins.readFile ./key.pub) ];
    };
    users.users.me = {
      isNormalUser = true;
      uid = 1000;
      password = "me";
      openssh.authorizedKeys.keys = [ (builtins.readFile ./key.pub) ];
      packages = [
        chromium
        firefox
        sway-run
        cage-run
        i3-run
      ] ++ (with pkgs; [
        foot
        vim
        cage
        htop
        i3
        git
        tmux
        nodejs
        python3
        python3Packages.pip
        supabase-cli
      ]);
    };

    programs = {
      sway = {
        enable = true;
        extraPackages = [
          pkgs.dmenu
          pkgs.xwayland
          pkgs.i3status
        ];
      };
      chromium = {
        enable = true;
        extensions = [
          "cjpalhdlnbpafiamejdnhcphjbkeiagm"
        ];
        extraOpts = {
          RestoreOnStartup = 5;
          HttpsOnlyMode = "force_enabled";
          DefaultSearchProviderEnabled = true;
          DefaultSearchProviderName = "DuckDuckGo";
          DefaultSearchProviderSearchURL = "https://duckduckgo.com/?q={searchTerms}";
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

    services = {
      openssh = {
        enable = true;
	settings = {
          PasswordAuthentication = false;
	};
      };
      cage = {
        enable = false;
        program = "${chromium}/bin/${chromium-exe}";
        user = "me";
      };
    };

    documentation.enable = false;

    hardware.opengl = {
      enable = true;
    };

    system.stateVersion = "23.05";

    systemd.services.enusre-home-me = {
      description = "Ensure /home/me exists and is owned by me";
      after = [ "local-fs.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = ensure-home-me;
        RemainAfterExit = "yes";
      };
    };

  };

}
