{ config, pkgs, ... }:

let

  home-manager-snapshot = import ./home-manager-snapshot.nix pkgs;

  linuxDesktopPkgs = with pkgs; [
    dmenu
    i3
    i3status
    i3lock
    pavucontrol
  ];

in

{

  home.packages = (with pkgs; [
    ag
    alacritty
    bat
    cage
    ctags
    curl
    diskus
    exa
    fd
    feh
    file
    git
    gnupg
    google-chrome
    grim
    hexyl
    htop
    jetbrains.idea-community
    jq
    libinput
    manpages
    miniserve
    moreutils
    mpv
    nix-prefetch-github
    nodejs
    openjdk
    p7zip
    pastel
    pciutils
    pstree
    pwgen
    python3
    ripgrep
    rsync
    sbt
    slurp
    sqlite
    st
    tmux
    tre
    tree
    unzip
    usbutils
    v4l_utils
    vscode
    wget
    youtube-dl
    zip
  ])
  ++ (with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    fast-tags
    ghc
    ghcid
    hasktags
    hpack
    nix-derivation
    nix-diff
  ])
  ++ linuxDesktopPkgs
  ;

  programs = {
    home-manager = {
      enable = true;
      path = "${home-manager-snapshot}";
    };
    vim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [
        vim-sensible
        vim-commentary
        vim-surround
        vim-unimpaired
        vim-fugitive
        vim-repeat
        vim-rsi
        vim-sleuth
        vim-vinegar
        vim-characterize
        vim-eunuch
        vim-abolish
        vim-obsession

        ack-vim
        # auto-pairs
        ctrlp-vim
        nerdtree
        psc-ide-vim
        purescript-vim
        vim-ctrlp-tjump
        vim-grepper
        vim-indent-object
        vim-nix
        vim-scala
        vim-sneak
      ];
      extraConfig = builtins.readFile ../../.vimrc;
    };
    bash = {
      enable = true;
      sessionVariables = {
        EDITOR = "vim";
        NIX_PATH = "nixpkgs=$HOME/src/NixOS/nixpkgs";
        JAVA_HOME = pkgs.jdk;
      };
      initExtra = with builtins; concatStringsSep "\n" (map readFile [ ../../.bashrc ../../external/z/z.sh ]);
    };
    firefox = {
      enable = true;
      package = pkgs.firefox-wayland;
      profiles = {
        me = {
          userChrome = ''
            #tabbrowser-tabs {
              visibility: collapse !important;
            }
          '';
        };
      };
    };
    alacritty = {
      enable = true;
      settings = {
        font = {
          size = 9;
        };
      };
    };
    broot = {
      enable = true;
      enableBashIntegration = true;
    };
  };

  home.file = {
    ".inputrc".source = ../../.inputrc;
    ".tmux.conf".source = ../../.tmux.conf;
    ".environment".source = ../../.environment;
    ".gitconfig".source = ../../.gitconfig;
  };

  xdg.configFile = {
    "sway/config".source = ../../.config/sway/config;
    "i3/config".source = ../../.config/i3/config;
    "i3status/config".source = ../../.config/i3status/config;
    "youtube-dl/config".source = ../../.config/youtube-dl/config;
    "mpv/mpv.conf".source = ../../.config/mpv/mpv.conf;
  };
}
