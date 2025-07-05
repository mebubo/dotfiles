{ pkgs ? import <nixpkgs> {} }:

let
  hpkgs = pkgs.haskell.packages.ghc910;
  # hpkgs = pkgs.haskellPackages;
in
pkgs.mkShell {
  buildInputs = [
    hpkgs.ghc
    hpkgs.cabal-install
    hpkgs.haskell-language-server
    pkgs.zlib
    pkgs.zlib.dev
    pkgs.pkg-config
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [
      pkgs.zlib
    ]}
    # export PKG_CONFIG_PATH=${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH
  '';
}
