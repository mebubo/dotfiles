self: super:

{
  user-packages = {
    nx-rebuild = super.writeScriptBin "nx-rebuild" ''
      #!${super.stdenv.shell}
      exec ${self.nix}/bin/nix-env -f '<nixpkgs>' -r -iA user-packages "$@"
    '';

  } // (with self; {
    inherit
    ag
    ctags
    curl
    dmenu
    feh
    file
    firefox-bin
    git
    gnupg
    google-chrome
    htop
    i3status
    jq
    moreutils
    mpv
    pavucontrol
    pstree
    pwgen
    ripgrep
    sbt-extras
    sqlite
    st
    sway-beta
    tmux
    tree
    unzip
    usbutils
    vim
    wget
    wlroots
    youtube-dl
    zip
    ;
    idea-community = jetbrains.idea-community_2018_2;
  }) // (with self.haskell.packages.ghc863; {
    inherit
    cabal-install
    cabal2nix
    ghcid
    hasktags
    hpack
    nix-derivation
    stack
    ;
  });
}
