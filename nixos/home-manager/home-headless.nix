{ config, pkgs, lib, ... }:

let

  home-manager-snapshot = import ./home-manager-snapshot.nix;

  ghcide-nix = pkgs.writeShellScriptBin "ghcide-nix" ''
    nix-shell --run "ghcide --lsp"
  '';

  haskell-sources-tags-nix = pkgs.fetchFromGitHub {
    owner = "mebubo";
    repo = "haskell-sources-tags-nix";
    rev = "9fdbfdc55de30933a209392d0fb320b2591b9d86";
    sha256 = "12bmw3gisn9fbi89lbhph52k8zab2jfly774frm0ql48h7lmaqcz";
    fetchSubmodules = false;
  };

  cabal-sources-tags = pkgs.callPackage (haskell-sources-tags-nix + "/cabal-sources-tags.nix") {};

in

{

  home.packages = (with pkgs; [
    ag
    bat
    cabal-sources-tags
    coursier
    ctags
    curl
    diskus
    exa
    fd
    feh
    file
    fzf
    git
    gnupg
    ghcide-nix
    grim
    hexyl
    htop
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
    perl
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
    wget
    youtube-dl
    zip
  ])
  ++ (with pkgs; [ gcc gnumake binutils ])
  ++ (with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    fast-tags
    ghc
    ghcid
    ghcide
    hasktags
    hpack
    nix-derivation
    nix-diff
    dhall
    dhall-json
    dhall-lsp-server
  ]);

  programs = {
    home-manager = {
      enable = true;
      path = "${home-manager-snapshot}";
    };
    neovim = {
      enable = true;
      vimAlias = true;
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
        # ctrlp-vim
        fzf-vim
        nerdtree
        psc-ide-vim
        purescript-vim
        # vim-ctrlp-tjump
        vim-grepper
        vim-indent-object
        vim-nix
        vim-scala
        vim-sneak
        dhall-vim

        nvim-lspconfig
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
        NIX_PATH = "nixpkgs=$HOME/src/NixOS/nixpkgs:nixpkgs-overlays=$HOME/src/me/dotfiles/nixos/overlays";
        HOME_MANAGER_CONFIG = "$HOME/src/me/dotfiles/home-manager/home.nix";
        JAVA_HOME = pkgs.jdk;
      };
      initExtra = with builtins; concatStringsSep "\n" (map readFile [ ../../.bashrc ../../external/z/z.sh ]);
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

  targets.genericLinux.enable = true;

  nixpkgs.overlays = [
    (import ../overlays/50-haskell.nix)
    (import ../overlays/50-home-manager.nix)
    (import ../overlays/50-purescript)
    (import ../overlays/50-st)
    (import ../overlays/50-vim-plugins.nix)
    (import ../overlays/50-neovim.nix)
  ];

  nixpkgs.config.allowUnfree = true;
}
