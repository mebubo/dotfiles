{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles-private = {
      url = "path:/home/me/src/me/dotfiles-private";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, dotfiles-private }: {
    nixosConfigurations = {
      laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (import ./nixos/devices/laptop/configuration.nix { inherit dotfiles-private; })
        ];
      };
      fr = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (import ./nixos/devices/fr/configuration.nix { inherit dotfiles-private; })
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.me = import ./nixos/home-manager/home.nix;
          }
        ];
      };
    };
  };
}
