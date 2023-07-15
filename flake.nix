{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-apple-silicon = {
      url = "github:tpwrules/nixos-apple-silicon";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles-private = {
      url = "path:/home/me/src/me/dotfiles-private";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, nixos-apple-silicon, dotfiles-private }:

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

    overlays = { nixpkgs.overlays = [
        (import ./nixos/overlays/50-vim-plugins.nix)
        (import ./nixos/overlays/50-intellij.nix)
      ];
    };

    # until I convert it to a flake
    dot-private = import dotfiles-private;

    private = {
      me.private = dot-private;
    };

  in {
    nixosConfigurations = {
      laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          private
          ./nixos/modules/me.nix
          ./nixos/devices/laptop/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.me = home-manager-module-nixos;
            home-manager.users.dev = home-manager-module-nixos;
            home-manager.users.dev2 = home-manager-module-nixos;
          }
          overlays
        ];
      };
      me = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          private
          ./nixos/modules/me.nix
          ./nixos/devices/me/configuration.nix
          nixos-apple-silicon.nixosModules.apple-silicon-support
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.me = home-manager-module-nixos;
          }
          overlays
        ];
      };
      raspberry = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          private
          ./nixos/modules/me.nix
          ./nixos/devices/raspberry/configuration.nix
        ];
      };
      fr = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          private
          ./nixos/modules/me.nix
          ./nixos/devices/fr/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.me = home-manager-module-nixos;
          }
          overlays
        ];
      };
    };
    homeConfigurations.me = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      modules = [
        home-manager-module-darwin
        overlays
      ];

    };
  };
}
