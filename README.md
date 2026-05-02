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
NIXPKGS_ALLOW_UNFREE=1 nix --extra-experimental-features 'nix-command flakes' build --impure '.#homeConfigurations.mb.activationPackage'
result/activate
```

Remove old home-manager generations:

```
nix-env --profile ~/.local/state/nix/profiles/home-manager --delete-generations 1d
```

why-depends:

```
SYS=$(nix eval --raw .#nixosConfigurations.fr.config.system.build.toplevel.drvPath)
nix-store -qR "$SYS" | grep rusty
nix why-depends --derivation $SYS /nix/store/4sj3ibfxnfzj3m4g1ljwiyg76h9dpd4b-rusty-v8-147.2.1.drv
```
