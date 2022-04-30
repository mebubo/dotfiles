#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded
oops() {
    echo "$0:" "$@" >&2
    exit 1
}

umask 0022

tmpDir="$(mktemp -d -t nix-binary-tarball-unpack.XXXXXXXXXX || \
          oops "Can't create temporary directory for downloading the Nix binary tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

require_util() {
    command -v "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

case "$(uname -s).$(uname -m)" in
    Linux.x86_64)
        hash=0b32afd8c9147532bf8ce8908395b1b4d6dde9bedb0fcf5ace8b9fe0bd4c075c
        path=0zij5bm5f2gm3p2c8dkkv58684j1k100/nix-2.8.0-x86_64-linux.tar.xz
        system=x86_64-linux
        ;;
    Linux.i?86)
        hash=3f4bb50f639515df069fb682bb68da77565e5ca8678a3b0fb7dcc79ef591f518
        path=kjpj1rn6x5lh20fkyfyyzgmgjdra1jpy/nix-2.8.0-i686-linux.tar.xz
        system=i686-linux
        ;;
    Linux.aarch64)
        hash=d29ea31c581e1ba7a651e6b22999cef8923e852e1d6fe7008d9545f4275f5343
        path=npadny2da5149lcycbfmacf1r936n9zg/nix-2.8.0-aarch64-linux.tar.xz
        system=aarch64-linux
        ;;
    Linux.armv6l_linux)
        hash=69d5cb0e95bc83154099debd139d4f767622d94b17149fa127d492017c2e3896
        path=jb1l7y40im5dsbq5gamppss59y0c7jmj/nix-2.8.0-armv6l-linux.tar.xz
        system=armv6l-linux
        ;;
    Linux.armv7l_linux)
        hash=25857729f23dc25fe92dabd376917d83fe0f23038f82c1f2ab230171eb70f648
        path=firp24ikxcygwrwd4208lyla4b6jl3sh/nix-2.8.0-armv7l-linux.tar.xz
        system=armv7l-linux
        ;;
    Darwin.x86_64)
        hash=ebf383f1b499d3e4897cd61d068dc46e118e5f53667f5f28748b0b3682d7649a
        path=wwf7b61nyhgj3z0vvgnnb4yzi081jkjp/nix-2.8.0-x86_64-darwin.tar.xz
        system=x86_64-darwin
        ;;
    Darwin.arm64|Darwin.aarch64)
        hash=f320f381299e0fc2f907ae81ac123d0689245cb39f0672f8a65dffea12fa0240
        path=fr5rcinvqzgcrggxw3phrzcck9wpzz83/nix-2.8.0-aarch64-darwin.tar.xz
        system=aarch64-darwin
        ;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

# Use this command-line option to fetch the tarballs using nar-serve or Cachix
if [ "${1:-}" = "--tarball-url-prefix" ]; then
    if [ -z "${2:-}" ]; then
        oops "missing argument for --tarball-url-prefix"
    fi
    url=${2}/${path}
    shift 2
else
    url=https://releases.nixos.org/nix/nix-2.8.0/nix-2.8.0-$system.tar.xz
fi

tarball=$tmpDir/nix-2.8.0-$system.tar.xz

require_util tar "unpack the binary tarball"
if [ "$(uname -s)" != "Darwin" ]; then
    require_util xz "unpack the binary tarball"
fi

if command -v curl > /dev/null 2>&1; then
    fetch() { curl --fail -L "$1" -o "$2"; }
elif command -v wget > /dev/null 2>&1; then
    fetch() { wget "$1" -O "$2"; }
else
    oops "you don't have wget or curl installed, which I need to download the binary tarball"
fi

echo "downloading Nix 2.8.0 binary tarball for $system from '$url' to '$tmpDir'..."
fetch "$url" "$tarball" || oops "failed to download '$url'"

if command -v sha256sum > /dev/null 2>&1; then
    hash2="$(sha256sum -b "$tarball" | cut -c1-64)"
elif command -v shasum > /dev/null 2>&1; then
    hash2="$(shasum -a 256 -b "$tarball" | cut -c1-64)"
elif command -v openssl > /dev/null 2>&1; then
    hash2="$(openssl dgst -r -sha256 "$tarball" | cut -c1-64)"
else
    oops "cannot verify the SHA-256 hash of '$url'; you need one of 'shasum', 'sha256sum', or 'openssl'"
fi

if [ "$hash" != "$hash2" ]; then
    oops "SHA-256 hash mismatch in '$url'; expected $hash, got $hash2"
fi

unpack=$tmpDir/unpack
mkdir -p "$unpack"
tar -xJf "$tarball" -C "$unpack" || oops "failed to unpack '$url'"

script=$(echo "$unpack"/*/install)

[ -e "$script" ] || oops "installation script is missing from the binary tarball!"
export INVOKED_FROM_INSTALL_IN=1
"$script" "$@"

} # End of wrapping
