#!/bin/bash

USERNAME=mebubo
INIT_DIR=$(cd $(dirname $0); pwd)
DOTFILES=$(dirname $INIT_DIR)
DOTFILES_ROOT=$DOTFILES/root

for f in $(cd $DOTFILES_ROOT/etc/apt; find . -type f); do
    cp $DOTFILES_ROOT/etc/apt/$f /etc/apt/$f
done

aptitude -y update
aptitude -y upgrade
aptitude -y install python
aptitude -y install $($INIT_DIR/read_list.py $INIT_DIR/packages)

for f in $(cd $DOTFILES_ROOT; find . -type f); do
    d=$(dirname /$f)
    mkdir -p $d
    cp $DOTFILES_ROOT/$f /$f
done

update-grub
