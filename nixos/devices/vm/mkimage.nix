let

  nixos = import <nixpkgs/nixos> {
    configuration = ./configuration.nix;
  };

  config = nixos.config;
  pkgs = nixos.pkgs;
  lib = pkgs.lib;

  img = import <nixpkgs/nixos/lib/make-disk-image.nix> {
    inherit config pkgs lib;
    # diskSize = 3000;
    format = "raw";
    onlyNixStore = true;
    installBootLoader = false;
    copyChannel = true;
    partitionTableType = "none";
  };

  imgFile = "${img}/nixos.img";

  port = "60022";

  home = "home.raw";

  start = name: args: pkgs.writeShellScriptBin "start-vm-${name}" ''
    ls -lah ${img}
    test -f ${home} || ( ${pkgs.qemu}/bin/qemu-img create -f raw ${home} 64G && ${pkgs.e2fsprogs}/bin/mkfs.ext4 -L home ${home} )
    # test -f ${home} || ( ${pkgs.qemu}/bin/qemu-img create -f qcow2 ${home} 64G )
    ${pkgs.qemu_kvm}/bin/qemu-kvm \
    -m 16G \
    -cpu host \
    -smp cpus=8 \
    -kernel ${config.system.build.toplevel}/kernel \
    -initrd ${config.system.build.toplevel}/initrd \
    -append "$(cat ${config.system.build.toplevel}/kernel-params) init=${config.system.build.toplevel}/init console=ttyS0" \
    -device virtio-keyboard \
    -usb -device usb-tablet,bus=usb-bus.0 \
    -audiodev pa,id=snd0 \
    -device ich9-intel-hda \
    -device hda-output,audiodev=snd0 \
    -nic user,hostfwd=tcp::${port}-:22 \
    -drive file=${imgFile},format=raw,id=nix-store,if=none,index=0,snapshot=on \
    -device virtio-blk-pci,drive=nix-store \
    -drive file=${home},format=raw,id=home,if=none,index=1 \
    -device virtio-blk-pci,drive=home \
    ${args}
  '';

  start-aarch64 = name: args: pkgs.writeShellScriptBin "start-vm-${name}" ''
    ls -lah ${img}
    test -f ${home} || ${pkgs.qemu}/bin/qemu-img create ${home} 4G
    ${pkgs.qemu}/bin/qemu-system-aarch64 \
    -snapshot \
    -machine virt,gic-version=max,accel=kvm:tcg \
    -m 5G \
    -cpu host \
    -smp cpus=1 \
    -kernel ${config.system.build.toplevel}/kernel \
    -initrd ${config.system.build.toplevel}/initrd \
    -append "$(cat ${config.system.build.toplevel}/kernel-params) init=${config.system.build.toplevel}/init" \
    -device virtio-gpu-pci \
    -device virtio-keyboard \
    -device usb-ehci,id=usb0 \
    -device usb-kbd \
    -device usb-tablet \
    -audiodev pa,id=snd0 \
    -device ich9-intel-hda \
    -device hda-output,audiodev=snd0 \
    -nic user,hostfwd=tcp::${port}-:22 \
    -drive file=${home},id=home,if=none,index=2 \
    -device virtio-blk-pci,drive=home \
    ${args} \
    ${imgFile}
  '';

  build-vm-aarch64-cmd = ''/nix/store/9ml8dxddg3hbdvlja5ys31zrkb3bfz4a-qemu-host-cpu-only-7.0.0/bin/qemu-system-aarch64
    -machine virt,gic-version=max,accel=kvm:tcg
    -cpu max
    -name nixos
    -m 1024
    -smp 1
    -device virtio-rng-pci
    -net nic,netdev=user.0,model=virtio
    -netdev user,id=user.0,
    -virtfs local,path=/nix/store,security_model=none,mount_tag=nix-store
    -virtfs local,path=/tmp/nix-vm.hSzkmpBa6B/xchg,security_model=none,mount_tag=shared
    -virtfs local,path=/tmp/nix-vm.hSzkmpBa6B/xchg,security_model=none,mount_tag=xchg
    -drive cache=writeback,file=/home/me/src/me/nixos-boxes/vm-manual/nixos.qcow2,id=drive1,if=none,index=1,werror=report
    -device virtio-blk-pci,drive=drive1
    -device virtio-keyboard
    -device virtio-gpu-pci
    -device usb-ehci,id=usb0
    -device usb-kbd
    -device usb-tablet
    -kernel /nix/store/an8s7dc97xq32jfzn9vl545d6z4fjqc4-nixos-system-nixos-22.11.git.38860c9e91c/kernel
    -initrd /nix/store/an8s7dc97xq32jfzn9vl545d6z4fjqc4-nixos-system-nixos-22.11.git.38860c9e91c/initrd
    -append loglevel=4 net.ifnames=0 init=/nix/store/an8s7dc97xq32jfzn9vl545d6z4fjqc4-nixos-system-nixos-22.11.git.38860c9e91c/init regInfo=/nix/store/jqm8zal56bd2v6ys5kxkpl2vvxqr7l5s-closure-info/registration console=ttyAMA0,115200n8 console=tty0'';

  ssh-vm = user: pkgs.writeShellScriptBin "ssh-vm-${user}" ''
    ${pkgs.openssh}/bin/ssh -o StrictHostKeyChecking=no -i key -p ${port} ${user}@localhost
  '';

  sshfs-vm = user: pkgs.writeShellScriptBin "sshfs-vm-${user}" ''
    ${pkgs.sshfs}/bin/sshfs -o StrictHostKeyChecking=no -o IdentityFile=key -p ${port} ${user}@localhost:src vm-src
  '';

in

  pkgs.symlinkJoin {
    name = "start-vm";
    paths = [
      (start "vesa" "")
      (start "gl-sdl" "-device virtio-vga-gl -display sdl,gl=on")
      (start "gl-gtk" "-device virtio-vga-gl -display gtk,gl=on")
      (start "qxl" "-vga qxl")
      (start "std" "-vga std")
      (start "text" "-nographic")
      (start-aarch64 "aarch64" "-display sdl")
      (ssh-vm "me")
      (sshfs-vm "me")
      (ssh-vm "root")
    ];
  }
