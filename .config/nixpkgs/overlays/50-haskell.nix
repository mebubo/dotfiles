self: super:
with super.haskell.lib;
{
  h = super.haskell.packages.ghc863;

  myHaskellEnv = super.haskell.packages.ghc863.ghcWithPackages
  (haskellPackages: with haskellPackages; [
    mtl transformers lens recursion-schemes
  ]);

}
