self: super:

{
  haskellPackages = super.haskellPackages.override {
    overrides = slf: sup:
    let
      repline = slf.repline_0_4_0_0.override { haskeline = slf.haskeline_0_8_1_0; };
      dhall-default = sup.dhall;
    in
    {
      haskeline_0_8_1_0 = self.haskell.lib.dontCheck sup.haskeline_0_8_1_0;
      dhall-default = sup.dhall;
      dhall = self.haskell.lib.dontCheck (slf.dhall_1_34_0.override { inherit repline; });
      dhall-json = self.haskell.lib.doJailbreak slf.dhall-json_1_7_1;
      dhall-lsp-server = slf.dhall-lsp-server_1_0_9;
      spago = sup.spago.override { dhall = dhall-default; };
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
