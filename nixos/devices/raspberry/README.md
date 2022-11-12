```
nixos-generate -f sd-aarch64 --system aarch64-linux -I nixpkgs=$HOME/src/NixOS/nixpkgs -c configuration.nix
```

```
nix-build "<nixpkgs/nixos>" -A config.system.build.sdImage --system aarch64-linux -I nixpkgs=$HOME/src/NixOS/nixpkgs -I nixos-config=configuration.nix -j 1
```

```
nix build -f "<nixpkgs/nixos>" config.system.build.sdImage --system aarch64-linux -I nixpkgs=$HOME/src/NixOS/nixpkgs -I nixos-config=configuration.nix -j 1
```

```
vcgencmd measure_clock arm
vcgencmd measure_temp
echo on > /sys/devices/platform/soc/*v3d/power/control
```
