self: super:

let

  home-manager-snapshot = import ../home-manager-snapshot.nix;

in

{
  home-manager-init = self.callPackage "${home-manager-snapshot}/home-manager" {
    path = "${home-manager-snapshot}";
  };
}
