{ ... }:

let

  home-manager-shapshot = import ../../../.config/nixpkgs/home-manager-snapshot.nix;

in

{
  imports = [ "${home-manager-shapshot}/nixos" ];

  home-manager = {
    # useGlobalPkgs = true;
    users.me = import ../../../.config/nixpkgs/home.nix;
    users.dev = import ../../../.config/nixpkgs/home.nix;
    users.dev2 = import ../../../.config/nixpkgs/home.nix;
  };
}
