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
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dotfiles-private = {
      url = "path:/home/me/src/me/dotfiles-private";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, nixos-apple-silicon, nix-darwin, dotfiles-private }:

  let

    home-manager-user-nixos = { ... }: {
      imports = [
        ./nixos/home-manager/home-headless.nix
        ./nixos/home-manager/home-desktop.nix
      ];
    };

    home-manager-user-darwin = { ... }: {
      imports = [
        ./nixos/home-manager/home-headless.nix
        ./nixos/home-manager/home-darwin.nix
      ];
    };

    home-manager-module = users: user-module: {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users = nixpkgs.lib.genAttrs users (u: user-module);
      };
    };

    overlays = {
      nixpkgs.overlays = [
        (import ./nixos/overlays/50-vim-plugins.nix)
        (import ./nixos/overlays/50-intellij.nix)
      ];
    };

    private = {
      me.private = import dotfiles-private;
    };

  in {
    nixosConfigurations = {

      laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          private
          ./nixos/modules/me.nix
          ./nixos/devices/laptop/configuration.nix
          home-manager.nixosModules.home-manager
          (home-manager-module ["me" "dev" "dev2"] home-manager-user-nixos)
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
          home-manager.nixosModules.home-manager
          (home-manager-module ["me"] home-manager-user-nixos)
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
          home-manager.nixosModules.home-manager
          (home-manager-module ["me"] home-manager-user-nixos)
          overlays
        ];
      };

    };

    homeConfigurations.me = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      modules = [
        home-manager-user-darwin
        overlays
      ];

    };

    darwinConfigurations.mba = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./nixos/devices/mba/configuration.nix
        home-manager.darwinModules.home-manager
        (home-manager-module ["me"] home-manager-user-darwin)
        overlays
      ];
    };

    packages."aarch64-darwin" =
      let
        pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      in
        rec {
          darwin-rebuild = nix-darwin.packages."aarch64-darwin".darwin-rebuild;
          darwin-activate = pkgs.writeShellScriptBin "darwin-activate" ''
            ${self.darwinConfigurations.mba.system}/activate
            nix-env -p /nix/var/nix/profiles/system --set ${self.darwinConfigurations.mba.system}
          '';
          darwin-chsh-me = pkgs.writeShellScriptBin "darwin-chsh-me" ''
            sudo chsh -s /run/current-system/sw/bin/bash me
          '';
          darwin-activate-user = pkgs.writeShellScriptBin "darwin-activate-user" ''
            ${self.darwinConfigurations.mba.system}/activate-user
          '';
          darwin-all = pkgs.linkFarmFromDrvs "darwin-all" [
            darwin-activate
            darwin-chsh-me
            darwin-activate-user
          ];
        };
  };
}
