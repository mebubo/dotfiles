self: super:
with super.haskell.lib;
{
  haskellPackages = super.haskellPackages // {
    codex = doJailbreak super.haskellPackages.codex;
    heist = dontCheck super.haskellPackages.heist;
  };

  myHaskellEnv = super.haskell.packages.ghc862.ghcWithPackages
  (haskellPackages: with haskellPackages; [
    mtl transformers lens recursion-schemes
  ]);

}
