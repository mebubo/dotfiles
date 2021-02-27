{ ... }:

let

  home-manager-shapshot = import ../../home-manager/home-manager-snapshot.nix;

in

{
  imports = [ "${home-manager-shapshot}/nixos" ];

  home-manager = {
    # useGlobalPkgs = true;
    users.me = import ../../home-manager/home.nix;
    users.dev = import ../../home-manager/home.nix;
    users.dev2 = import ../../home-manager/home.nix;
  };
}
