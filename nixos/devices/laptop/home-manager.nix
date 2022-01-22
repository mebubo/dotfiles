{ ... }:

let

  home-manager-shapshot = import ../../home-manager/home-manager-snapshot.nix;

in

{
  imports = [ "${home-manager-shapshot}/nixos" ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      me = import ../../home-manager/home.nix;
      dev = import ../../home-manager/home.nix;
      dev2 = import ../../home-manager/home.nix;
    };
  };
}
