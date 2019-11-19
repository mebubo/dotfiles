#!/usr/bin/env bash

nix-build --attr system --arg configuration $1 ./nixos.nix
