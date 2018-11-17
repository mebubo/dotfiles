self: super:

{
  kanshi = super.callPackage ./kanshi.nix {};
  wlroots = super.wlroots.overrideAttrs (oldAttrs: rec {
    src = super.fetchFromGitHub {
      owner = "swaywm";
      repo = "wlroots";
      rev = "040d62de0076a349612b7c2c28c5dc5e93bb9760";
      sha256 = "0iyr0xcyh46z78aa3sqxkapb11afvrjk2b0mhq1pxg66da7mxxiv";
    };
    patches = [];
    name = "wlroots-git";
  });
  sway-beta = super.sway-beta.overrideAttrs (oldAttrs: rec {
    src = super.fetchFromGitHub {
      owner = "swaywm";
      repo = "sway";
      rev = "4ce18d2744776b6789f7118bc08d44ae424ca6b3";
      sha256 = "13q3mfvdhgzijxxpc42cnja7crqbnmsfj8kpw20jah5h8i8i45wv";
    };
    name = "sway-git";
    mesonFlags = "-Dsway-version=sway-git-${src.rev}";
  });
}
