#!/bin/bash

DOTFILES=$(dirname $BASH_SOURCE)

$DOTFILES/install-nix-2.3.10.sh --no-daemon

. $HOME/.nix-profile/etc/profile.d/nix.sh

NIX_PATH=$NIX_PATH:nixpkgs-overlays=$DOTFILES/nixos/overlays nix-shell -p home-manager-init --run "HOME_MANAGER_CONFIG=$DOTFILES/nixos/home-manager/home.nix home-manager switch"
