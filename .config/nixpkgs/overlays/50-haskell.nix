self: super:
with super.haskell.lib;
{
  haskellPackages = super.haskellPackages // {
    codex = super.haskellPackages.callPackage /home/me/src/codex {};
    grid = doJailbreak super.haskellPackages.grid;
    heist = dontCheck super.haskellPackages.heist;
    hasktags = dontCheck super.haskellPackages.hasktags;
  };

  myHaskellEnv = super.haskell.packages.ghc862.ghcWithPackages
  (haskellPackages: with haskellPackages; [
    mtl transformers lens recursion-schemes
  ]);

}
