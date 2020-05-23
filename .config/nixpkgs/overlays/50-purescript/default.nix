let

  pkgs = import ./nixpkgs.nix {};

  purescript = pkgs.haskellPackages.callPackage ./purescript.nix {
    happy = pkgs.haskellPackages.happy_1_19_9;
  };

in

  self: super: {
    purescript-pinned = with pkgs.haskell.lib; generateOptparseApplicativeCompletion "purs" (doJailbreak (dontCheck (dontHaddock purescript)));
  }
