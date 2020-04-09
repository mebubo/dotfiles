let

  pkgs = import ./nixpkgs.nix {};

in

  self: super: {
    purescript-pinned = pkgs.haskellPackages.purescript;
  }
