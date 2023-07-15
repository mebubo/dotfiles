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

  outputs = { self, nixpkgs, home-manager, dotfiles-private }:

  let
    home-manager-module-nixos = { ... }: {
      imports = [
        ./nixos/home-manager/home-headless.nix
        ./nixos/home-manager/home-desktop.nix
      ];
    };
    home-manager-module-darwin = { ... }: {
      imports = [
        ./nixos/home-manager/home-headless.nix
        ./nixos/home-manager/home-darwin.nix
      ];
    };

    # until I convert it to a flake
    dot-private = import dotfiles-private;

  in {
    nixosConfigurations = {
      laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (import ./nixos/devices/laptop/configuration.nix { dotfiles-private = dot-private; })
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.me = home-manager-module-nixos;
            home-manager.users.dev = home-manager-module-nixos;
            home-manager.users.dev2 = home-manager-module-nixos;
          }
        ];
      };
      fr = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (import ./nixos/devices/fr/configuration.nix { dotfiles-private = dot-private; })
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.me = home-manager-module-nixos;
          }
        ];
      };
    };
    homeConfigurations.me = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      modules = [
        home-manager-module-darwin
      ];

    };
  };
}
