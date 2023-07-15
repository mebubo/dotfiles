{ ... }:

let

  home-manager-shapshot = import ../home-manager/home-manager-snapshot.nix;

in

{
  imports = [ "${home-manager-shapshot}/nixos" ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      me = import ../home-manager/home.nix;
    };
  };

  nixpkgs.overlays = [
    (import ../overlays/50-home-manager.nix)
    (import ../overlays/50-vim-plugins.nix)
  ];

}
