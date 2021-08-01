self: super:

{
  haskellPackages = super.haskellPackages.override {
    overrides = slf: sup: {
    };
  };
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc865 = super.haskell.packages.ghc865.override {
        overrides = slf: sup: {
        };
      };
    };
  };
}
