{ pkgs, bubblewrap, obsidian, coreutils, busybox, xdg-utils }:

pkgs.writeShellScriptBin "obsidian" ''
    ${coreutils}/bin/mkdir -p $HOME/notes
    ${coreutils}/bin/mkdir -p $HOME/.config/obsidian
    ${bubblewrap}/bin/bwrap \
        --ro-bind /nix /nix \
        --proc /proc \
        --dev /dev \
        --unshare-all \
        --setenv PATH ${busybox}/bin:${obsidian}/bin:${xdg-utils}/bin \
        --setenv XDG_RUNTIME_DIR $XDG_RUNTIME_DIR \
        --setenv LANG C.UTF-8 \
        --setenv USER $USER \
        --setenv HOME $HOME \
        --setenv WAYLAND_DISPLAY $WAYLAND_DISPLAY \
        --setenv NIXOS_OZONE_WL $NIXOS_OZONE_WL \
        --setenv XDG_SESSION_TYPE $XDG_SESSION_TYPE \
        --setenv XDG_CURRENT_DESKTOP $XDG_CURRENT_DESKTOP \
        --setenv XCURSOR_SIZE $XCURSOR_SIZE \
        --setenv XCURSOR_PATH $XCURSOR_PATH \
        --setenv XCURSOR_THEME $XCURSOR_THEME \
        --dir $HOME/.cache \
        --ro-bind /etc/fonts /etc/fonts \
        --ro-bind /etc/profiles /etc/profiles \
        --ro-bind /etc/localtime /etc/localtime \
        --ro-bind /run/user/"$(id -u)"/"$WAYLAND_DISPLAY" /run/user/"$(id -u)"/"$WAYLAND_DISPLAY" \
        --ro-bind $HOME/.local/share $HOME/.local/share \
        --bind $HOME/notes $HOME/notes \
        --bind $HOME/.config/obsidian $HOME/.config/obsidian \
        ${obsidian}/bin/obsidian
        # ${busybox}/bin/sh
''
