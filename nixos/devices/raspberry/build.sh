#!/bin/sh

nix-build "<nixpkgs/nixos>" -A config.system.build.sdImage --system aarch64-linux -I nixpkgs=$HOME/src/NixOS/nixpkgs -I nixos-config=configuration.nix -j 1
