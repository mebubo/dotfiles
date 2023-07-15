{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    dotfiles-private = {
      url = "path:/home/me/src/me/dotfiles-private";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, dotfiles-private }: {
    nixosConfigurations = {
      laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos/devices/laptop/configuration.nix
        ];
      };
      fr = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (import ./nixos/devices/fr/configuration.nix { inherit dotfiles-private; })
        ];
      };
    };
  };
}
