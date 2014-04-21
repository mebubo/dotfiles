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

# /sbin/dosfslabel does not work
DRIVE_LABEL=MULTIBOOT

DATE_PATTERN="+%Y%m%d-%H%M%S"

ISO_DIR=$MOUNTPOINT/boot/iso

DEBIAN_DIR_REL=boot/debian
DEBIAN_DIR=$MOUNTPOINT/$DEBIAN_DIR_REL

UBUNTU_SERVER_DIR_REL=boot/ubuntu-server
UBUNTU_SERVER_DIR=$MOUNTPOINT/$UBUNTU_SERVER_DIR_REL

DEBIAN_DAILY_DIR=$DEBIAN_DIR

set_vars () {
    UBUNTU_ISO="http://releases.ubuntu.com/$RELEASE/ubuntu-$RELEASE-desktop-$ARCH.iso"

    UBUNTU_SERVER_KERNEL="http://archive.ubuntu.com/ubuntu/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/ubuntu-installer/$ARCH/linux"
    UBUNTU_SERVER_INITRD="http://archive.ubuntu.com/ubuntu/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/ubuntu-installer/$ARCH/initrd.gz"

    DEBIAN_KERNEL="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/debian-installer/$ARCH/linux"
    DEBIAN_INITRD="http://ftp.debian.org/debian/dists/$RELEASE/main/installer-$ARCH/current/images/netboot/debian-installer/$ARCH/initrd.gz"

    DEBIAN_DAILY_KERNEL="http://d-i.debian.org/daily-images/$ARCH/daily/netboot/debian-installer/$ARCH/linux"
    DEBIAN_DAILY_INITRD="http://d-i.debian.org/daily-images/$ARCH/daily/netboot/debian-installer/$ARCH/initrd.gz"

    FEDORA_ISO="http://download.fedoraproject.org/pub/fedora/linux/releases/$RELEASE/Live/$ARCH/Fedora-Live-Desktop-$ARCH-$RELEASE-1.iso"

    ARCH_ISO="http://mir.archlinux.fr/iso/$RELEASE/archlinux-$RELEASE-dual.iso"
}

UBUNTU_ARCHES=${UBUNTU_ARCHES-"amd64 i386"}
UBUNTU_RELEASES=${UBUNTU_RELEASES-"12.04.4 13.10 14.04"}

DEBIAN_ARCHES=${DEBIAN_ARCHES-"amd64 i386"}
DEBIAN_RELEASES=${DEBIAN_RELEASES-"stable"}

UBUNTU_SERVER_ARCHES=${UBUNTU_SERVER_ARCHES-"amd64 i386"}
UBUNTU_SERVER_RELEASES=${UBUNTU_SERVER_RELEASES-"trusty"}

DEBIAN_DAILY_ARCHES=${DEBIAN_ARCHES}
DEBIAN_DAILY_RELEASES="daily"

FEDORA_ARCHES=${FEDORA_ARCHES-"x86_64"}
FEDORA_RELEASES=${FEDORA_RELEASES-"19"}

ARCH_ARCHES=all
ARCH_RELEASES=${ARCH_RELEASES-"2014.04.01"}

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

    create_grub_cfg_debian
    DEBIAN_RELEASES=$DEBIAN_DAILY_RELEASES create_grub_cfg_debian
    create_grub_cfg_ubuntu_server
    create_grub_cfg_ubuntu
    create_grub_cfg_fedora
    create_grub_cfg_arch
}

create_grub_cfg_debian () {
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
}

create_grub_cfg_ubuntu_server () {
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
}

create_grub_cfg_ubuntu () {
    for f in $(cd $ISO_DIR; ls *ubuntu*.iso | sort -r); do
        local KERNEL
        case $f in
            ubuntu-*-amd64.iso)
                KERNEL=vmlinuz.efi
                ;;
            *)
                KERNEL=vmlinuz
                ;;
        esac
        cat >> $GRUB_CFG <<EOF
menuentry "$f" {
  loopback loop /boot/iso/$f
  linux (loop)/casper/$KERNEL boot=casper iso-scan/filename=/boot/iso/$f noeject noprompt --
  initrd (loop)/casper/initrd.lz
}
EOF
    done
}

create_grub_cfg_fedora () {
    # https://bugzilla.redhat.com/show_bug.cgi?id=650672
    # http://git.marmotte.net/git/glim/plain/fedora18-fromiso
    # don't bother with scripts to modify initrd
    # I can mount it manually once a year
    for f in $(cd $ISO_DIR; ls Fedora*.iso | sort -r); do
        cat >> $GRUB_CFG <<EOF
menuentry "$f (at dracut prompt, manually mount -o loop /path/to/$f /isodevice)" {
  loopback loop /boot/iso/$f
  linux (loop)/isolinux/vmlinuz0 root=live:CDLABEL=${f::-1} initrd=initrd0.img rootfstype=auto ro rd.live.image rhgb rd.luks=0 rd.md=0 rd.dm=0
  initrd (loop)/isolinux/initrd0.img
}
EOF
    done
}

create_grub_cfg_arch () {
    for f in $(cd $ISO_DIR; ls archlinux-*.iso | sort -r); do

        local ISO_PATH="/boot/iso/$f"
        # archlinux-2013.06.01-dual.iso -> ARCH_201306
        local ISO_LABEL=ARCH_${f:10:4}${f:15:2}

        cat >> $GRUB_CFG <<EOF
menuentry "$f" {
  loopback loop $ISO_PATH
  linux (loop)/arch/boot/x86_64/vmlinuz archisobasedir=arch archisolabel=$ISO_LABEL img_dev=/dev/disk/by-label/$DRIVE_LABEL img_loop=$ISO_PATH earlymodules=loop noeject noprompt
  initrd (loop)/arch/boot/x86_64/archiso.img
}
EOF
    done
}

download () {
    download_iso UBUNTU
    download_debian_like DEBIAN
    download_debian_like DEBIAN_DAILY
    download_debian_like UBUNTU_SERVER
    download_iso FEDORA
    download_iso ARCH
}

download_iso () {
    mkdir -p $ISO_DIR

    local DISTRO=$1

    local RELEASES=${DISTRO}_RELEASES
    local ARCHES=${DISTRO}_ARCHES
    local ISO=${DISTRO}_ISO

    if [ -n "${!RELEASES}" -a -n "${!ARCHES}" ]; then
        for ARCH in ${!ARCHES}; do
            for RELEASE in ${!RELEASES}; do
                set_vars
                wget -P $ISO_DIR -c ${!ISO}
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
