{ config, pkgs, lib, ... }:

let

  home-manager-snapshot = import ./home-manager-snapshot.nix;

  haskell-sources-tags-nix = pkgs.fetchFromGitHub {
    owner = "mebubo";
    repo = "haskell-sources-tags-nix";
    rev = "9fdbfdc55de30933a209392d0fb320b2591b9d86";
    sha256 = "12bmw3gisn9fbi89lbhph52k8zab2jfly774frm0ql48h7lmaqcz";
    fetchSubmodules = false;
  };

  cabal-sources-tags = pkgs.callPackage (haskell-sources-tags-nix + "/cabal-sources-tags.nix") {};

  hm = pkgs.writeShellScriptBin "hm" ''
    # NIX_PATH=nixpkgs=$HOME/src/NixOS/nixpkgs:nixpkgs-overlays=$HOME/src/me/dotfiles/nixos/overlays ${pkgs.home-manager}/bin/home-manager -f $HOME/src/me/dotfiles/nixos/home-manager/home.nix "$@"
    NIX_PATH=nixpkgs=$HOME/src/NixOS/nixpkgs ${pkgs.home-manager}/bin/home-manager -f $HOME/src/me/dotfiles/nixos/home-manager/home.nix "$@"
  '';

in

{

  home.packages = (with pkgs; [
    silver-searcher
    alloy
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
    hexyl
    hledger
    hledger-ui
    hledger-web
    htop
    jq
    man-pages
    # metals
    # miniserve
    moreutils
    mpv
    nix-prefetch-git
    nix-prefetch-github
    nixos-generators
    nixos-shell
    nodejs
    openjdk
    # p7zip
    pastel
    pciutils
    perl
    pstree
    # purescript-pinned
    pwgen
    python3
    restic
    ripgrep
    rsync
    sbt
    spago
    sqlite
    tmux
    # tre
    tree
    unzip
    wget
    zip
  ])
  ++ (with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    # fast-tags
    ghc
    ghcid
    ghcide
    hasktags
    hpack
    nix-derivation
    # purescript
    nix-diff
    dhall
    dhall-json
    dhall-lsp-server
  ])
  ++ [
    cabal-sources-tags
    hm
  ]
  ;

  programs = {
    home-manager = {
      enable = true;
      path = "${home-manager-snapshot}";
    };
    git = {
      enable = true;
      extraConfig = builtins.readFile ../../.gitconfig;
      lfs = {
        enable = true;
      };
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

        base16-vim

        fzf-vim
        nerdtree
        ack-vim
        vim-indent-object
        vim-sneak
        # auto-pairs
        # ctrlp-vim
        # vim-ctrlp-tjump
        # vim-grepper

        dhall-vim
        purescript-vim
        # psc-ide-vim
        # vim-nix
        # vim-scala

        nvim-lspconfig
        nvim-metals
        (nvim-treesitter.withPlugins (p: [
          p.tree-sitter-nix
          p.tree-sitter-scala
          p.tree-sitter-haskell
          p.tree-sitter-rust
          p.tree-sitter-lua
          p.tree-sitter-java
          p.tree-sitter-javascript
          p.tree-sitter-typescript
          p.tree-sitter-python
          p.tree-sitter-markdown
          p.tree-sitter-bash
          p.tree-sitter-json
          p.tree-sitter-yaml
        ]))
        # treesitter playground
        playground

        telescope-nvim

      ];
      extraConfig = builtins.readFile ../../.vimrc;
      withNodeJs = false;
      withPython3 = false;
      withRuby = false;
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
        HOME_MANAGER_CONFIG = "$HOME/src/me/dotfiles/nixos/home-manager/home.nix";
        JAVA_HOME = pkgs.jdk;
        NIXOS_OZONE_WL = "1";
      };
      initExtra = ''
        ${builtins.readFile ../../.bashrc}

        . ${../../external/z/z.sh}

        # https://github.com/NixOS/nixpkgs/blob/d510b23805c37a5b11b86dc3ba8723fcaa6f4539/nixos/modules/programs/bash/bash-completion.nix#L23-L34
        if shopt -q progcomp &>/dev/null; then
          . "${pkgs.bash-completion}/etc/profile.d/bash_completion.sh"
          nullglobStatus=$(shopt -p nullglob)
          shopt -s nullglob
          for p in $NIX_PROFILES; do
            for m in "$p/etc/bash_completion.d/"*; do
              . "$m"
            done
          done
          eval "$nullglobStatus"
          unset nullglobStatus p m
        fi
      '';
      shellAliases = {
        l = "${pkgs.exa}/bin/exa -la";
      };
    };
    broot = {
      enable = false;
      enableBashIntegration = true;
    };
    exa = {
      enable = true;
      enableAliases = true;
    };
    fzf = {
      enable = true;
      enableBashIntegration = false;
    };
    mcfly = {
      enable = false;
      enableBashIntegration = true;
      enableFishIntegration = false;
      enableZshIntegration = false;
    };
    bat = {
      enable = true;
      config = {
        theme = "Solarized (light)";
      };
    };
    yt-dlp = {
      enable = true;
      settings = {
        output  = "%(uploader)s--%(upload_date)s--%(title)s--%(id)s.%(ext)s";
      };
    };
  };

  home.file = {
    ".inputrc".source = ../../.inputrc;
    ".tmux.conf".source = ../../.tmux.conf;
    # ".gitconfig".source = ../../.gitconfig;
  };

  nix = {
    enable = true;
    package = pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
    };
  };

  manual.manpages.enable = true;

  home.stateVersion = "22.05";

}
