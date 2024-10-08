{ config, lib, inputs, pkgs, ... }:

{

  users.users = {
    me = {
      home = "/Users/me";
      shell = pkgs.bashInteractive;
    };
  };

  networking = {
    computerName = "mba";
    hostName = "mba";
  };

  system.defaults = {

    dock.autohide = true;
    dock.mru-spaces = false;
    dock.minimize-to-application = true;
    dock.show-recents = false;

    dock.orientation = "left";
    dock.showhidden = true;

    dock.tilesize = 32;

    dock.wvous-tr-corner = 1;
    dock.wvous-br-corner = 1;
    dock.wvous-tl-corner = 1;
    dock.wvous-bl-corner = 1;

    spaces.spans-displays = false;
    screencapture.location = "/tmp";

    trackpad.Clicking = true;
    trackpad.TrackpadThreeFingerDrag = true;

    finder.AppleShowAllExtensions = true;
    finder.FXEnableExtensionChangeWarning = false;
    finder.CreateDesktop = false;
    finder.FXPreferredViewStyle = "Nlsv"; # list view
    finder.ShowPathbar = true;
    finder.QuitMenuItem = true;

    loginwindow.GuestEnabled = false;

    CustomUserPreferences = {
      # 3 finger dragging
      "com.apple.AppleMultitouchTrackpad".DragLock = false;
      "com.apple.AppleMultitouchTrackpad".Dragging = false;
      "com.apple.AppleMultitouchTrackpad".TrackpadThreeFingerDrag = true;

      # Finder's default location upon open
      "com.apple.finder".NewWindowTargetPath = "file://${config.users.users.me.home}/";
    };

    NSGlobalDomain.AppleICUForce24HourTime = true;
    NSGlobalDomain.AppleInterfaceStyle = null;
    NSGlobalDomain.AppleInterfaceStyleSwitchesAutomatically = false;
    NSGlobalDomain.AppleShowScrollBars = "WhenScrolling";
    NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
    NSGlobalDomain."com.apple.trackpad.scaling" = 3.0;
    NSGlobalDomain."com.apple.swipescrolldirection" = false;

    NSGlobalDomain.AppleKeyboardUIMode = 3;
    NSGlobalDomain.ApplePressAndHoldEnabled = false;
    NSGlobalDomain.InitialKeyRepeat = 10;
    NSGlobalDomain.KeyRepeat = 1;
    NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
    NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;
    NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;
    NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;
    NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;
    NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
    NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;
    NSGlobalDomain._HIHideMenuBar = false;

  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  environment.systemPackages =
    [
      pkgs.vim
      pkgs.curl
      pkgs.git
      pkgs.htop
      pkgs.jq
      pkgs.ripgrep
    ];

  fonts = {
    packages = [ pkgs.monaspace ];
  };

  security.pam.enableSudoTouchIdAuth = false;

  services.yabai.enable = false;
  services.yabai.package = pkgs.yabai;
  services.skhd.enable = false;

  services.nix-daemon.enable = true;

  nix.extraOptions = ''
    gc-keep-derivations = true
    gc-keep-outputs = true
    experimental-features = nix-command flakes
  '';

  programs.bash = {
    enable = true;
    enableCompletion = true;
    interactiveShellInit = ''
    '';
  };

  programs.zsh = {
    enable = true;
  };

  environment.variables.LANG = "en_US.UTF-8";

  nixpkgs.config.allowUnfree = true;

  nixpkgs.hostPlatform = "aarch64-darwin";

  nix.configureBuildUsers = true;
  nix.nrBuildUsers = 32;
}
