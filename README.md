home-manager standalone (macos):

```sh
NIXPKGS_ALLOW_UNFREE=1 nix --extra-experimental-features 'nix-command flakes' build --impure '.#homeConfigurations.me.activationPackage'
result/activate
```
