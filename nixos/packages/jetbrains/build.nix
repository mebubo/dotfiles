let
  pkgs = import <nixpkgs> {};
  jdk = pkgs.callPackage ./jetbrains-jdk.nix {};

in pkgs.callPackage ./intellij.nix { inherit jdk; }
