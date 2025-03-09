NixOS build:

```
nixos-rebuild build --flake .#fr --print-build-logs --log-format bar-with-logs
```

NixOS install:

```
nixos-rebuild boot --flake .#fr
```

home-manager standalone (macos):

```sh
NIXPKGS_ALLOW_UNFREE=1 nix --extra-experimental-features 'nix-command flakes' build --impure '.#homeConfigurations.me.activationPackage'
result/activate
```

Remove old home-manager generations:

```
nix-env --profile ~/.local/state/nix/profiles/home-manager --delete-generations 1d
```
