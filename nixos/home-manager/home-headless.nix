{ config, pkgs, lib, ... }:

let

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
    silver-searcher
    curl
    diskus
    fd
    feh
    file
    fzf
    git
    gnupg
    hexyl
    htop
    jq
    man-pages
    moreutils
    mpv
    nix-prefetch-git
    nix-prefetch-github
    nixos-generators
    nixos-shell
    pastel
    pciutils
    pstree
    pwgen
    python3
    restic
    ripgrep
    rsync
    sqlite
    tmux
    tree
    unzip
    usbutils
    wget
    zip
  ])
  ++ (with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    ghc
    ghcid
    ghcide
    hasktags
    hpack
    nix-derivation
    nix-diff
    dhall
    dhall-json
  ])
  ++ [
    cabal-sources-tags
  ]
  ;

  programs = {
    home-manager = {
      enable = true;
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
      enableCompletion = true;
      sessionVariables = {
        EDITOR = "vim";
        LESS = "-r -F -X -j10";
        NIX_PATH = "nixpkgs=$HOME/src/NixOS/nixpkgs";
        JAVA_HOME = pkgs.jdk;
        NIXOS_OZONE_WL = "1";
      };
      initExtra = ''
        ${builtins.readFile ../../.bashrc}
      '';
      shellAliases = {
        l = "ls -la";
      };
    };
    broot = {
      enable = false;
      enableBashIntegration = true;
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
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
    };
  };

  manual.manpages.enable = true;

  home.stateVersion = "22.05";

}
