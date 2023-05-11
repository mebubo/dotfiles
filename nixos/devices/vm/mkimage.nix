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

  start = name: args:
    let
      console = if name == "text" then "console=ttyS0" else "";
    in
      pkgs.writeShellScriptBin "start-vm-${name}" ''
        test -f ${home} || ( ${pkgs.qemu}/bin/qemu-img create -f raw ${home} 64G && ${pkgs.e2fsprogs}/bin/mkfs.ext4 -L home ${home} )
        ${pkgs.qemu_kvm}/bin/qemu-kvm \
        -m 64G \
        -cpu host \
        -smp cpus=8 \
        -kernel ${config.system.build.toplevel}/kernel \
        -initrd ${config.system.build.toplevel}/initrd \
        -append "$(cat ${config.system.build.toplevel}/kernel-params) init=${config.system.build.toplevel}/init ${console}" \
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

  ssh-vm = user: pkgs.writeShellScriptBin "ssh-vm-${user}" ''
    ${pkgs.openssh}/bin/ssh -o StrictHostKeyChecking=no -i key -p ${port} ${user}@localhost
  '';

  sshfs-vm = user: pkgs.writeShellScriptBin "sshfs-vm-${user}" ''
    ${pkgs.sshfs}/bin/sshfs -o StrictHostKeyChecking=no -o IdentityFile=$(pwd)/key -p ${port} ${user}@localhost: vm-home
  '';

  start-aarch64 = (import ./aarch64.nix) { inherit pkgs config img imgFile home port; };

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
