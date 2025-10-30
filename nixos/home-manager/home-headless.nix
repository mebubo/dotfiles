{ config, pkgs, lib, ... }:

let

me-nixos-rebuild = pkgs.writeShellScriptBin "me-nixos-rebuild" ''
  set -u
  nixos-rebuild build --flake .#$1 --print-build-logs --log-format bar-with-logs
'';

me-home-manager-cleanup-old-generations = pkgs.writeShellScriptBin "me-home-manager-cleanup-old-generations" ''
  nix-env --profile ~/.local/state/nix/profiles/home-manager --delete-generations 1d
'';

me-home-manager-rebuild = pkgs.writeShellScriptBin "me-home-manager-rebuild" ''
  nix build '.#homeConfigurations.me.activationPackage'
  result/activate
'';

me-loudnorm = pkgs.writeShellScriptBin "me-loudnorm" ''
  set -euo pipefail

  if [ "$#" -eq 0 ]; then
    echo "Usage: me-loudnorm FILE..." >&2
    exit 1
  fi

  mkdir -p norm
  ${pkgs.parallel}/bin/parallel -j12 -q ${pkgs.ffmpeg}/bin/ffmpeg -i "{}" -vn -af loudnorm=I=-12:LRA=5:TP=-1.2 -c:a libmp3lame -q:a 2 "norm/{.}.mp3" ::: "$@"
'';

me-wayland-connect = pkgs.writeShellScriptBin "me-wayland-connect" ''
  export WAYLAND_DISPLAY=/run/user/1000/wayland-1
  export XDG_SESSION_TYPE=wayland
  export NIXOS_OZONE_WL=1
  exec "$@"
'';

me-wayland-xorg-connect = pkgs.writeShellScriptBin "me-wayland-xorg-connect" ''
  export DISPLAY=:1
  exec ${me-wayland-connect}/bin/me-wayland-connect "$@"
'';

in

{

  home.packages = (with pkgs; [
    curl
    dig
    diskus
    fd
    feh
    ffmpeg
    file
    fzf
    git
    gnupg
    hexyl
    htop
    btop
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
    kepubify
    qsv
    xan
    me-nixos-rebuild
    me-home-manager-cleanup-old-generations
    me-home-manager-rebuild
    me-loudnorm
    me-wayland-connect
    me-wayland-xorg-connect
  ]);

  programs = {
    home-manager = {
      enable = true;
    };
    git = {
      enable = true;
      lfs = {
        enable = true;
      };
      settings = {
        merge.conflictstyle = "diff3";
        user = {
          name = "Sergei Dolgov";
          email = "dolgovs@gmail.com";
        };
        alias = {
          co = "checkout";
          br = "branch";
          ci = "commit";
          st = "status";
          last = "log -1 HEAD";
          unstage = "reset HEAD --";
          trash = "!git add . && git commit -m 'trash' && git reset --hard HEAD^";
          serve = "!git daemon --reuseaddr --verbose --base-path=. --export-all --port=8000 ./.git";
          news = "log -p HEAD@{1}..HEAD@{0}";
          clonep = "!f() { git clone $1 $(echo $1 | sed 's;.*[/:]\\([^/]*/[^/]*\\).git;\\1;'); }; f";
        };
        difftool.prompt = false;
        color = {
          branch = "auto";
          diff = "auto";
          interactive = "auto";
          status = "auto";
        };
        push.default = "simple";
        pull.ff = "only";
        core.quotepath = false;
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
    zoxide = {
      enable = true;
      enableBashIntegration = true;
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
    helix = {
      enable = true;
      settings = {
        theme = "base16_default_dark";
        editor = {
          true-color = true;
        };
      };
    };
    jujutsu = {
      enable = true;
    };
    delta = {
      enable = true;
      enableGitIntegration = true;
      enableJujutsuIntegration = true;
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
