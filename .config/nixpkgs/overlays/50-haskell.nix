self: super:

{
  haskellPackages = super.haskellPackages.override {
    overrides = slf: sup:
    let
      repline = slf.repline_0_4_0_0.override { haskeline = slf.haskeline_0_8_0_0; };
      dhall-default = sup.dhall;
    in
    {
      dhall = self.haskell.lib.dontCheck (slf.dhall_1_33_1.override { inherit repline; });
      dhall-json = slf.dhall-json_1_7_0;
      dhall-lsp-server = slf.dhall-lsp-server_1_0_8;
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
