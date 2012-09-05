#!/bin/bash

set -e

PATTERN="/dev/sd[b-z]1"

case "$1" in
    $PATTERN)
        DEVICE=$1
        MOUNTPOINT=/media/$(basename $DEVICE)
        shift
        ;;
    *)
        echo "Usage: $0 $PATTERN"
        exit 1
        ;;
esac

DATE_PATTERN="+%Y%m%d-%H%M%S"

ISO_DIR=$MOUNTPOINT/boot/iso
DEBIAN_DIR=$MOUNTPOINT/boot/debian

set_vars () {
    UBUNTU="http://releases.ubuntu.com/12.04/ubuntu-12.04.1-desktop-$ARCH.iso"
    DEBIAN_KERNEL="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-amd64/current/images/netboot/debian-installer/amd64/linux"
    DEBIAN_INITRD="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-amd64/current/images/netboot/debian-installer/amd64/initrd.gz"
}

UBUNTU_ARCHES="i386 amd64"
DEBIAN_RELEASES="stable testing unstable"

install_grub () {
    sudo grub-install --no-floppy --root-directory=$MOUNTPOINT ${DEVICE::-1}
}

mount () {
    case $(pmount) in
        *$DEVICE*)
            echo "$DEVICE already mounted"
            ;;
        *)
            pmount $DEVICE
            ;;
    esac
}

umount () {
    pumount $DEVICE
}

create_grub_cfg () {
    GRUB_CFG=$MOUNTPOINT/boot/grub/grub.cfg
    mv -f $GRUB_CFG $GRUB_CFG.$(date $DATE_PATTERN)

    for RELEASE in $DEBIAN_RELEASES; do
        cat >> $GRUB_CFG <<EOF
menuentry "Debian $RELEASE $(date -r $MOUNTPOINT/boot/debian/$RELEASE/linux $DATE_PATTERN) netboot" {
  linux /boot/debian/$RELEASE/linux priority=low base-installer/install-recommends=false language=en country=FR locale=en_GB.UTF-8
  initrd /boot/debian/$RELEASE/initrd.gz
}
EOF
    done

    for f in $(cd $ISO_DIR; ls *ubuntu*.iso | sort -r); do
        cat >> $GRUB_CFG <<EOF
menuentry "$f" {
  loopback loop /boot/iso/$f
  linux (loop)/casper/vmlinuz boot=casper iso-scan/filename=/boot/iso/$f noeject noprompt --
  initrd (loop)/casper/initrd.lz
}
EOF
    done
}

download () {

    mkdir -p $ISO_DIR

    for ARCH in $UBUNTU_ARCHES; do
        set_vars
        wget -P $ISO_DIR -c $UBUNTU
    done

    for RELEASE in $DEBIAN_RELEASES; do
        mkdir -p $DEBIAN_DIR/$RELEASE
        set_vars
        wget -P $DEBIAN_DIR/$RELEASE -N $DEBIAN_KERNEL
        wget -P $DEBIAN_DIR/$RELEASE -N $DEBIAN_INITRD
    done
}

all () {
    mount
    install_grub
    download
    create_grub_cfg
    umount
}

update () {
    mount
    download
    create_grub_cfg
    umount
}

"$@"
