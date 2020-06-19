self: super:

{
  haskellPackages = super.haskellPackages.override {
    overrides = slf: sup: {
      dhall = slf.dhall_1_32_0;
      dhall-json = slf.dhall-json_1_6_4;
    };
  };
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc865 = super.haskell.packages.ghc865.override {
        overrides = slf: sup: {
          # time-compat = self.haskell.lib.doJailbreak sup.time-compat;
          # purescript = super.haskell.lib.doJailbreak (slf.callCabal2nix "purescript" /home/me/src/purescript/purescript {
          #   happy = sup.happy_1_19_9;
          # });
        };
      };
    };
  };
}
