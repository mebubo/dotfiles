self: super:

{
  user-packages = {
    nx-rebuild = super.writeScriptBin "nx-rebuild" ''
      #!${super.stdenv.shell}
      exec ${self.nix}/bin/nix-env -f '<nixpkgs>' -j1 -r -iA user-packages "$@"
    '';

  } // (with self; {
    inherit
    ag
    alacritty
    bat
    coursier
    ctags
    curl
    dmenu
    exa
    fd
    feh
    ffmpeg_4
    file
    firefox-wayland
    fish
    git
    gnupg
    google-chrome
    grim
    htop
    i3status
    jq
    miniserve
    moreutils
    mpv
    nodejs
    pavucontrol
    pstree
    pwgen
    ripgrep
    sbt
    sqlite
    slurp
    st
    sway
    swayidle
    swaylock
    tigervnc
    tmux
    tree
    unzip
    usbutils
    v4l_utils
    vim
    vlc
    vscode
    wget
    wlroots
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
    hoogle
    hpack
    nix-derivation
    stack
    ;
  });
}
