{ config, pkgs, lib, ... }:

{

  home.packages = (with pkgs; [
    silver-searcher
    curl
    dig
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
    minicom
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
    python313
    python313Packages.ipython
    restic
    ripgrep
    rsync
    sqlite
    sshfs
    tmux
    tree
    unzip
    wget
    zip
  ]);

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
      };
      initExtra = ''
        ${builtins.readFile ../../.bashrc}
      '';
      shellAliases = {
        l = "ls -lah";
        ll = "ls -la";
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
