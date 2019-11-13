test -f ~/.environment && . ~/.environment
test -f ~/.environment-private && . ~/.environment-private

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

shopt -s histappend
shopt -s checkwinsize

export MAILCHECK=-1
export HISTCONTROL=ignoreboth
export HISTSIZE=100000

# colorfull prompt:
# 30: Black/Dark grey
# 31: Red
# 32: Green
# 33: Yellow
# 34: Blue
# 35: Magenta
# 36: Fuscia
# 37: White/light grey
# 38: "Default" foreground color

case $HOSTNAME in
    laptop*)
        PS1='\[\033[01;36m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'
        ;;
    *)
        PS1='\[\033[01;34m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'
        ;;
esac

if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]
then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='exa --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

HISTORY_FILE=~/history
_append_history () {
    local LAST_CMD=$(history 1)
    # strip useless entry number from the beginning
    LAST_CMD=${LAST_CMD#*  }
    echo "$(date) -- cd $(pwd); $LAST_CMD" >> $HISTORY_FILE
}

_update_window_title () {
    echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"
}

_configure_show_current_command_in_window_title () {
    trap 'echo -ne "\033]0;$BASH_COMMAND\007"' DEBUG
}

case $TERM in
    uxterm*|xterm*|rxvt*|st*)
        PROMPT_COMMAND="_append_history; history -a; _update_window_title"
        ;;
    *)
        PROMPT_COMMAND="_append_history; history -a"
        ;;
esac

case $(tty) in
    /dev/tty?)
        TMOUT=300
        ;;
esac

alias ll='exa -l'
alias la='exa -A'
alias l='exa -Fhla'
alias less="less -j 5"
alias cat=bat

alias cp="cp --reflink=auto --sparse=always"

export GPG_TTY=`tty`

function rsync_mirror {
    rsync -avHP --inplace -e 'ssh -o ClearAllForwardings=yes' "$@"
}

function hdmi {
    case "$1" in
        on)
            xrandr --output HDMI-1 --auto
            pactl set-card-profile 0 output:hdmi-stereo
            ;;
        off)
            xrandr --output HDMI-1 --off
            pactl set-card-profile 0 output:analog-stereo+input:analog-stereo
            ;;
        *)
            echo "Usage: hdmi <on|off>"
            return 1
            ;;
    esac
}

function copy_terminfo {
  local REMOTE_HOST=$1
  infocmp | ssh $REMOTE_HOST tic -
}

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
function _resolve_this_dir {
    local SOURCE="${BASH_SOURCE[0]}"
    while [ -h "$SOURCE" ]; do
        # resolve $SOURCE until the file is no longer a symlink
        DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
        SOURCE="$(readlink "$SOURCE")"
        # if $SOURCE was a relative symlink, we need to resolve it
        # relative to the path where the symlink file was located
        [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
    done
    cd -P "$( dirname "$SOURCE" )" && pwd
}

_THIS_DIR=$(_resolve_this_dir)

Z=$_THIS_DIR/external/z/z.sh
test -f $Z && . $Z

function nx-haskell {
  pkgs=${@}
  nix-shell -I nixpkgs=$HOME/src/NixOS/nixpkgs -p "h.ghcWithPackages (pkgs: with pkgs; [$pkgs])"
}

function nx-hash {
  local hash=$1
  nix-hash --type sha256 --to-base32 $hash
}

function nx-hash-file {
  local file=$1
  nix-hash --type sha256 --base32 $file
}

function nx-add-root-drv {
  local f=$1
  nix-instantiate $f --indirect --add-root $f.drv
}

function nx-print-roots {
  nix-store --gc --print-roots
}

function nx-closure {
  nix-store -q --requisites $1
}

function nx-roots {
  nix-store -q --roots $1
}

function nx-referrers {
  nix-store -q --referrers $1
}

function nx-build {
  nix-build -E 'with import <nixpkgs> {  }; callPackage ./default.nix {  }'
}

function nx-build-haskell {
  nix-build -E 'with import <nixpkgs> {  }; h.callPackage ./default.nix {  }'
}
