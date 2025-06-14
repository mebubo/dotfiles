#!/usr/bin/env bash

export NIXOS_OZONE_WL=1
export WAYLAND_DISPLAY=wayland-1
export XDG_RUNTIME_DIR=/home/dev/xdg
export XDG_SESSION_TYPE=wayland

$@
