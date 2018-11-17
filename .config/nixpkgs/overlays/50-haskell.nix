self: super:
with super.haskell.lib;
{
  haskellPackages = super.haskellPackages // {
    codex = doJailbreak super.haskellPackages.codex;
    grid = doJailbreak super.haskellPackages.grid;
    heist = dontCheck super.haskellPackages.heist;
    hasktags = dontCheck super.haskellPackages.hasktags;
  };

  myHaskellEnv = super.haskell.packages.ghc862.ghcWithPackages
  (haskellPackages: with haskellPackages; [
    mtl transformers lens recursion-schemes
  ]);

}
