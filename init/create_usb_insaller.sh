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

DEBIAN_DIR_REL=boot/debian
DEBIAN_DIR=$MOUNTPOINT/$DEBIAN_DIR_REL

UBUNTU_SERVER_DIR_REL=boot/ubuntu-server
UBUNTU_SERVER_DIR=$MOUNTPOINT/$UBUNTU_SERVER_DIR_REL

set_vars () {
    UBUNTU="http://releases.ubuntu.com/$RELEASE/ubuntu-$RELEASE-desktop-$ARCH.iso"

    UBUNTU_SERVER_KERNEL="http://archive.ubuntu.com/ubuntu/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/ubuntu-installer/$ARCH/linux"
    UBUNTU_SERVER_INITRD="http://archive.ubuntu.com/ubuntu/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/ubuntu-installer/$ARCH/initrd.gz"

    DEBIAN_KERNEL="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/debian-installer/$ARCH/linux"
    DEBIAN_INITRD="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/debian-installer/$ARCH/initrd.gz"
}

UBUNTU_ARCHES=${UBUNTU_ARCHES-"amd64 i386"}
UBUNTU_RELEASES=${UBUNTU_RELEASES-"12.10 12.04.2"}

DEBIAN_ARCHES=${DEBIAN_ARCHES-"amd64 i386"}
DEBIAN_RELEASES=${DEBIAN_RELEASES-"stable testing unstable"}

UBUNTU_SERVER_ARCHES=${UBUNTU_SERVER_ARCHES-"amd64 i386"}
UBUNTU_SERVER_RELEASES=${UBUNTU_SERVER_RELEASES-"precise"}

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
menuentry "Debian $ARCH $RELEASE  netboot $(date -r $DEBIAN_DIR/$ARCH/$RELEASE/linux $DATE_PATTERN)" {
  linux /$DEBIAN_DIR_REL/$ARCH/$RELEASE/linux priority=low base-installer/install-recommends=false language=en country=FR locale=en_GB.UTF-8
  initrd /$DEBIAN_DIR_REL/$ARCH/$RELEASE/initrd.gz
}
EOF
        done
    done

    for ARCH in $UBUNTU_SERVER_ARCHES; do
        for RELEASE in $UBUNTU_SERVER_RELEASES; do
            cat >> $GRUB_CFG <<EOF
menuentry "Ubuntu server $ARCH $RELEASE  netboot $(date -r $UBUNTU_SERVER_DIR/$ARCH/$RELEASE/linux $DATE_PATTERN)" {
  set gfxpayload=keep
  linux /$UBUNTU_SERVER_DIR_REL/$ARCH/$RELEASE/linux tasks=standard pkgsel/language-pack-patterns= pkgsel/install-language-support=false priority=low base-installer/install-recommends=false language=en country=FR locale=en_GB.UTF-8 --
  initrd /$UBUNTU_SERVER_DIR_REL/$ARCH/$RELEASE/initrd.gz
}
EOF
        done
    done

    for f in $(cd $ISO_DIR; ls *ubuntu*.iso | sort -r); do
        local KERNEL=vmlinuz
        # for some reason the name of the kernel is different in this
        # particular iso
        if [ "$f" = "ubuntu-12.04.2-desktop-amd64.iso" ]; then
            KERNEL=vmlinuz.efi
        fi
        cat >> $GRUB_CFG <<EOF
menuentry "$f" {
  loopback loop /boot/iso/$f
  linux (loop)/casper/$KERNEL boot=casper iso-scan/filename=/boot/iso/$f noeject noprompt --
  initrd (loop)/casper/initrd.lz
}
EOF
    done
}

download () {
    download_ubuntu
    download_debian_like DEBIAN
    download_debian_like UBUNTU_SERVER
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

download_debian_like () {
    mkdir -p $ISO_DIR

    local DISTRO=$1

    local RELEASES=${DISTRO}_RELEASES
    local ARCHES=${DISTRO}_ARCHES
    local DIR=${DISTRO}_DIR
    local KERNEL=${DISTRO}_KERNEL
    local INITRD=${DISTRO}_INITRD

    if [ -n "${!RELEASES}" -a -n "${!ARCHES}" ]; then
        for ARCH in ${!ARCHES}; do
            for RELEASE in ${!RELEASES}; do
                mkdir -p ${!DIR}/${ARCH}/${RELEASE}
                set_vars
                wget -P ${!DIR}/${ARCH}/${RELEASE} -N ${!KERNEL}
                wget -P ${!DIR}/${ARCH}/${RELEASE} -N ${!INITRD}
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
