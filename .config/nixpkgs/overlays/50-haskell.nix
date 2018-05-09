self: super:
with super.haskell.lib;
{
  haskellPackages = super.haskellPackages // {
    codex = doJailbreak super.haskellPackages.codex;
    heist = dontCheck super.haskellPackages.heist;
  };

}
