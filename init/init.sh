#!/bin/bash

set -e

INIT_DIR=$(cd $(dirname $0); pwd)
DOTFILES_DEFAULT_DIR=$(dirname $INIT_DIR)
DOTFILES_DIR=${DOTFILES_DIR-$DOTFILES_DEFAULT_DIR}
SYSTEM_DOTFILES_DIR=$DOTFILES_DIR/root

TIMESTAMP=$(date +%Y%m%d%H%M%S)

files_differ () {
    ! diff "$1" "$2" &> /dev/null
}

run () {
    echo Running "$@"
    "$@"
}

copy_tree () {

    local CREATE_BACKUP=true
    local LIST=SRC
    local FILTER='*'
    local METHOD="cp -f"

    while [ $# -gt 2 ]; do
	    case "$1" in
	        --no-backup)
		        CREATE_BACKUP=false
		        shift
		        ;;
	        --list-files-in-dest)
		        LIST=DEST
		        shift
		        ;;
	        --filter)
		        FILTER="$2"
		        shift 2
		        ;;
            --symlink)
                METHOD="ln -s"
                shift
                ;;
	        *)
		        echo Unrecognized option "$1"
		        return 1
		        ;;
	    esac
    done

    local SRC="$1"
    local DEST="$2"

    # resolve indirect refernce
    LIST=${!LIST}

    while read FILE; do
	    case "$FILE" in
            ./.git/*)
                continue
                ;;
	        ./$FILTER)
		        ;;
	        *)
		        echo Filtered out "$FILE"
		        continue
		        ;;
	    esac

	    DEST_FILE="$DEST"/"$FILE"
	    DEST_DIR=$(dirname "$DEST_FILE")
	    SRC_FILE=$SRC/$FILE
	    [ -d "$DEST_DIR" ] || mkdir -p "$DEST_DIR"
	    if [ -f "$DEST_FILE" ]; then
	        if files_differ "$SRC_FILE" "$DEST_FILE"; then
		        if [ "$CREATE_BACKUP" = "true" ]; then
		            run cp -L "$DEST_FILE" "$DEST_FILE".$TIMESTAMP
                    rm -f "$DEST_FILE"
		        fi
		        run $METHOD "$SRC_FILE" "$DEST_FILE"
	        else
		        echo Not copying, identical "$SRC_FILE" "$DEST_FILE"
	        fi
	    else
	        run $METHOD "$SRC_FILE" "$DEST_FILE"
	    fi
    done < <(cd $LIST; find . -type f)
}


read_packages () {
    for LIST in $@; do
        cat $INIT_DIR/$LIST | while read PACKAGE; do
	        case $PACKAGE in
	            "" | \#*)
		            ;;
	            *)
		            echo $PACKAGE
		            ;;
	        esac
        done
    done
}

configure_apt () {
    copy_tree $SYSTEM_DOTFILES_DIR/etc/apt /etc/apt
}

package_lists_old () {
    touch -d '-1 hour' /tmp/1h_ago.timestamp
    test /var/lib/apt/lists -ot /tmp/1h_ago.timestamp
}

install () {
    if package_lists_old; then
        aptitude -y update
        aptitude -y upgrade
    else
        echo Package lists are recent, not updating
    fi
    aptitude -y install $@
}

install_base () {
    install $(read_packages packages-base)
}

install_all () {
    install $(read_packages packages-base packages-gui)
}

populate_system_configs () {
    copy_tree $SYSTEM_DOTFILES_DIR /
    update-grub
}

populate_user_configs () {
    copy_tree --filter '.*' --symlink $DOTFILES_DIR $HOME
}

refresh_system_configs_in_repo () {
    copy_tree --no-backup --list-files-in-dest / $SYSTEM_DOTFILES_DIR
}

system_base () {
    configure_apt
    install_base
    populate_system_configs
}

system_all () {
    configure_apt
    install_all
    populate_system_configs
}

user () {
    populate_user_configs
}

"$@"
