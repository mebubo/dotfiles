systemImage:

{ pkgs, lib, qemu, e2fsprogs }:

let

  rootFilesystemLabel = "nixos";

  # based on https://github.com/NixOS/nixpkgs/blob/982a9bcc79d338e3cd177c9d109dbac5b5569750/nixos/modules/virtualisation/qemu-vm.nix#L87
  startVM =
    ''
      set -e

      # Create an empty ext4 filesystem image. A filesystem image does not
      # contain a partition table but just a filesystem.
      createEmptyFilesystemImage() {
        local name=$1
        local size=$2
        local temp=$(mktemp)
        ${qemu}/bin/qemu-img create -f raw "$temp" "$size"
        ${e2fsprogs}/bin/mkfs.ext4 -L ${rootFilesystemLabel} "$temp"
        ${qemu}/bin/qemu-img convert -f raw -O qcow2 "$temp" "$name"
        rm "$temp"
      }

      NIX_DISK_IMAGE=$(readlink -f "''${NIX_DISK_IMAGE:-nixos.qcow2}") || test -z "$NIX_DISK_IMAGE"

      if test -n "$NIX_DISK_IMAGE" && ! test -e "$NIX_DISK_IMAGE"; then
          echo "Disk image do not exist, creating the virtualisation disk image..."

          # Create a writable qcow2 image using the systemImage as a backing image.
          ${qemu}/bin/qemu-img create \
            -f qcow2 \
            -b ${systemImage}/nixos.qcow2 \
            -F qcow2 \
            "$NIX_DISK_IMAGE"
          echo "Virtualisation disk image created."
      fi

      # Create a directory for storing temporary data of the running VM.
      if [ -z "$TMPDIR" ] || [ -z "$USE_TMPDIR" ]; then
          TMPDIR=$(mktemp -d nix-vm.XXXXXXXXXX --tmpdir)
      fi

      # Create a directory for exchanging data with the VM.
      mkdir -p "$TMPDIR/xchg"

      cd "$TMPDIR"

      # Start QEMU.
      exec ${qemu}/bin/qemu-kvm \
        -cpu max \
        -name vm2 \
        -m 32G \
        -smp 8 \
        -nographic \
        -device virtio-rng-pci \
        -nic user,hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:8080,hostfwd=tcp::8000-:8000 \
        -drive cache=writeback,file=$NIX_DISK_IMAGE,id=drive1,if=none,index=1,werror=report \
        -device virtio-blk-pci,bootindex=1,drive=drive1,serial=root \
        -device virtio-keyboard \
        -usb -device usb-tablet,bus=usb-bus.0
    '';

in

  pkgs.writeShellScriptBin "run-nixos-vm" startVM
