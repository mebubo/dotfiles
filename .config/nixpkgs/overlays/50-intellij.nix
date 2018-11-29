self: super:

{
  jetbrains = super.jetbrains // {
    idea-community = super.jetbrains.idea-community.overrideAttrs (oldAttrs: rec {
      version = "2018.2.6";
      name = "idea-community-${version}";
      src = super.fetchurl {
        url = "https://download.jetbrains.com/idea/ideaIC-${version}.tar.gz";
        sha256 = "02hpbyivji9vnik7p04zrja1rhhl49r0365g0i6sa1rrwd1fhvwf";
      };
    });
  };
}
