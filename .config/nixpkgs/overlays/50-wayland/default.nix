self: super:

{
  kanshi = super.callPackage ./kanshi.nix {};
  wlroots = super.wlroots.overrideAttrs (oldAttrs: rec {
    src = super.fetchFromGitHub {
      owner = "swaywm";
      repo = "wlroots";
      rev = "0.2";
      sha256 = "0gfxawjlb736xl90zfv3n6zzf5n1cacgzflqi1zq1wn7wd3j6ppv";
    };
    patches = [];
    name = "wlroots-git";
  });
  sway-beta = super.sway-beta.overrideAttrs (oldAttrs: rec {
    src = super.fetchFromGitHub {
      owner = "swaywm";
      repo = "sway";
      rev = "1.0-beta.2";
      sha256 = "0f9rniwizbc3vzxdy6rc47749p6gczfbgfdy4r458134rbl551hw";
    };
    name = "sway-git";
    mesonFlags = "-Dsway-version=sway-git-${src.rev}";
  });
}
