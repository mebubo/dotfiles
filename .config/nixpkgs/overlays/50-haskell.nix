self: super:
with super.haskell.lib;
{
  h = super.haskell.packages.ghc865;

  myHaskellEnv = self.h.ghcWithPackages
  (haskellPackages: with haskellPackages; [
    mtl transformers lens recursion-schemes
  ]);

}
