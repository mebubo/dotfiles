self: super:

{
  user-packages = {
    nx-rebuild = super.writeScriptBin "nx-rebuild" ''
      #!${super.stdenv.shell}
      exec ${self.nix}/bin/nix-env -f '<nixpkgs>' -j1 -r -iA user-packages "$@"
    '';

    xauth = self.xorg.xauth;
    xorgserver = self.xorg.xorgserver;

  } // (with self; {
    inherit
    ag
    alacritty
    bat
    cage
    # chromium
    coursier
    ctags
    curl
    dmenu
    # evince
    exa
    fd
    feh
    ffmpeg_4
    file
    firefox-wayland
    # fish
    git
    gnupg
    google-chrome
    grim
    htop
    i3
    i3status
    i3lock
    jq
    libinput
    miniserve
    moreutils
    mpv
    nix-prefetch-github
    nodejs
    pax-rs
    p7zip
    pavucontrol
    pstree
    pwgen
    python3
    ripgrep
    rsync
    sbt
    sqlite
    slurp
    st
    # sway
    tigervnc
    tmux
    tree
    unzip
    usbutils
    v4l_utils
    vim
    virt-viewer
    # vlc
    vscode
    waybar
    wget
    wlroots
    xpra
    youtube-dl
    zip
    ;
    # idea-community = jetbrains.idea-community;
  }) // (with self.h; {
    inherit
    cabal-install
    cabal2nix
    ghcid
    hasktags
    # hoogle
    hpack
    nix-derivation
    ghc
    fast-tags
    nix-diff
    # ghcide
    # stack
    ;
  });
}
