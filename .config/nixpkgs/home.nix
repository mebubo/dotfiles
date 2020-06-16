{ config, pkgs, lib, ... }:

let

  home-manager-snapshot = import ./home-manager-snapshot.nix pkgs;

  linuxDesktopPkgs = with pkgs; [
    cage
    dmenu
    google-chrome
    i3
    i3lock
    i3status
    pavucontrol
    wlr-randr
  ];

  chromeos-scale = pkgs.writeShellScriptBin "chromeos-scale" ''
    sommelier -X --dpi=160 --scale=1.3 "$@";
  '';

  chromeOSWrappers = {
    inherit chromeos-scale;
    idea-community-chromeos = pkgs.writeShellScriptBin "idea-community-chromeos" ''${chromeos-scale}/bin/chromeos-scale ${pkgs.jetbrains.idea-community}/bin/idea-community "$@"'';
    code-chromeos = pkgs.writeShellScriptBin "code-chromeos" ''${chromeos-scale}/bin/chromeos-scale ${pkgs.vscode}/bin/code -w "$@"'';
  };

in

{

  home.packages = (with pkgs; [
    ag
    bat
    coursier
    ctags
    curl
    # dhall
    # dhall-json
    diskus
    exa
    fd
    feh
    file
    fzf
    git
    gnupg
    grim
    hexyl
    htop
    jetbrains.idea-community
    jq
    libinput
    manpages
    # miniserve
    moreutils
    mpv
    niv
    nix-prefetch-git
    nix-prefetch-github
    nixos-generators
    nodejs
    openjdk
    # p7zip
    pastel
    pciutils
    pstree
    purescript-pinned
    pwgen
    python3
    ripgrep
    rsync
    sbt
    slurp
    spago
    sqlite
    st
    tmux
    # tre
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
  ]) ++ (
    with pkgs.haskellPackages;
    with pkgs.haskell.lib;
    let
      dhall = dhall_1_32_0;
      dhall-json = dhall-json_1_6_4.override { inherit dhall; };
      dhall-lsp-server = pkgs.haskellPackages.dhall-lsp-server.override { inherit dhall dhall-json; };
    in

    map justStaticExecutables [ dhall dhall-json dhall-lsp-server ]
  )
  ++ linuxDesktopPkgs
  # ++ (lib.attrValues chromeOSWrappers)
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
        fzf-vim
        nerdtree
        psc-ide-vim
        purescript-vim
        vim-ctrlp-tjump
        vim-grepper
        vim-indent-object
        vim-nix
        vim-scala
        vim-sneak
        dhall-vim
      ];
      extraConfig = builtins.readFile ../../.vimrc;
    };
    emacs = {
      enable = false;
      package = pkgs.emacs-nox;
      extraPackages = epkgs: with epkgs; [ org-roam ];
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
    fzf = {
      enable = true;
      enableBashIntegration = false;
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

  targets.genericLinux.enable = false;
}