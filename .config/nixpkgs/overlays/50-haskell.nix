self: super:

{
  haskellPackages = super.haskellPackages.override {
    overrides = slf: sup:
    let
      repline = slf.repline_0_4_0_0.override { haskeline = slf.haskeline_0_8_1_0; };
      dhall-default = sup.dhall;
    in
    {
      # haskeline_0_8_1_0 = self.haskell.lib.dontCheck sup.haskeline_0_8_1_0;
      # dhall-default = sup.dhall;
      # dhall = self.haskell.lib.dontCheck (slf.dhall_1_36_0.override { inherit repline; });
      # dhall-json = self.haskell.lib.doJailbreak slf.dhall-json_1_7_3;
      # dhall-lsp-server = slf.dhall-lsp-server_1_0_11;
      # spago = sup.spago.override { dhall = dhall-default; };
      # binary-instances = self.haskell.lib.dontCheck sup.binary-instances;

      # time-compat = slf.time-compat_1_9_4;
      # binary-instances = self.haskell.lib.dontCheck (sup.binary-instances.override {
      #   time-compat = slf.time-compat_1_9_4;
      #   aeson = self.haskell.lib.dontCheck (sup.aeson.override {
      #     time-compat = slf.time-compat_1_9_4;
      #   });
      # });
      # binary-instances = sup.binary-instances.override {
      #   quickcheck-instances = self.haskell.lib.doJailbreak (slf.quickcheck-instances_0_3_25.override {
      #     # QuickCheck = slf.QuickCheck_2_14_1.override {
      #     #   splitmix = slf.splitmix_0_1_0_3;
      #     # };
      #     data-fix = slf.data-fix_0_3_0;
      #     strict = slf.strict_0_4;
      #     time-compat = slf.time-compat_1_9_4;
      #   });
      #   time-compat = slf.time-compat_1_9_4;
      # };
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
