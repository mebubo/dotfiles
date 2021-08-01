self: super:

let

  inherit (self.haskell.lib) doJailbreak dontCheck dontHaddock unmarkBroken generateOptparseApplicativeCompletion;

in

  {

    haskellPackages = super.haskellPackages.override {
      overrides = slf: sup:
        let
          purescript-src = self.nix-gitignore.gitignoreSource [] ../../../../../purescript/purescript;
          purescript = self.haskellPackages.callCabal2nix "purescript" purescript-src {};
        in
          {
            purescript-cst = unmarkBroken (doJailbreak sup.purescript-cst);
            purescript = doJailbreak purescript;
          };
    };

    purescript = generateOptparseApplicativeCompletion "purs" self.haskellPackages.purescript;

  }
