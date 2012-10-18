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
        echo "Usage: $0 $PATTERN update|all"
        exit 1
        ;;
esac

DATE_PATTERN="+%Y%m%d-%H%M%S"

ISO_DIR=$MOUNTPOINT/boot/iso
DEBIAN_DIR=$MOUNTPOINT/boot/debian

set_vars () {
    UBUNTU="http://releases.ubuntu.com/$RELEASE/ubuntu-$RELEASE-desktop-$ARCH.iso"
    DEBIAN_KERNEL="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/debian-installer/$ARCH/linux"
    DEBIAN_INITRD="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/debian-installer/$ARCH/initrd.gz"
}

UBUNTU_ARCHES=${UBUNTU_ARCHES-"amd64 i386"}
UBUNTU_RELEASES=${DEBIAN_RELEASES-"12.10 12.04.1"}
DEBIAN_ARCHES=${DEBIAN_ARCHES-"amd64 i386"}
DEBIAN_RELEASES=${DEBIAN_RELEASES-"stable testing unstable"}

install_grub () {
    /usr/sbin/grub-install --no-floppy --root-directory=$MOUNTPOINT ${DEVICE::-1}
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
    [ -f $GRUB_CFG ] && mv -f $GRUB_CFG $GRUB_CFG.$(date $DATE_PATTERN)

    for ARCH in $DEBIAN_ARCHES; do
        for RELEASE in $DEBIAN_RELEASES; do
            cat >> $GRUB_CFG <<EOF
menuentry "Debian $ARCH $RELEASE  netboot $(date -r $MOUNTPOINT/boot/debian/$ARCH/$RELEASE/linux $DATE_PATTERN)" {
  linux /boot/debian/$ARCH/$RELEASE/linux priority=low base-installer/install-recommends=false language=en country=FR locale=en_GB.UTF-8
  initrd /boot/debian/$ARCH/$RELEASE/initrd.gz
}
EOF
        done
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
    download_ubuntu
    download_debian
}

download_ubuntu () {
    mkdir -p $ISO_DIR

    if [ -n "$UBUNTU_RELEASES" -a -n "$UBUNTU_ARCHES" ]; then
        for ARCH in $UBUNTU_ARCHES; do
            for RELEASE in $UBUNTU_RELEASES; do
                set_vars
                wget -P $ISO_DIR -c $UBUNTU
            done
        done
    fi
}

download_debian () {
    mkdir -p $ISO_DIR

    if [ -n "$DEBIAN_RELEASES" -a -n "$DEBIAN_ARCHES" ]; then
        for ARCH in $DEBIAN_ARCHES; do
            for RELEASE in $DEBIAN_RELEASES; do
                mkdir -p $DEBIAN_DIR/$ARCH/$RELEASE
                set_vars
                wget -P $DEBIAN_DIR/$ARCH/$RELEASE -N $DEBIAN_KERNEL
                wget -P $DEBIAN_DIR/$ARCH/$RELEASE -N $DEBIAN_INITRD
            done
        done
    fi
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
