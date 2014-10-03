# ~/.bashrc: executed by bash(1) for non-login shells.

. ~/.environment
test -f ~/.environment-private && . ~/.environment-private

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# append to the history file, don't overwrite it
shopt -s histappend

shopt -s cdspell

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

export MAILCHECK=-1
export HISTCONTROL=ignoredups
export HISTSIZE=100000

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh /usr/bin/lesspipe)"

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

if [ $SHELL = "/bin/bash" ]
then
    case $TERM in
        uxterm*|xterm*|rxvt*|linux|screen*)
            case $HOSTNAME in
                mayo|britany)
                    PS1='\[\033[01;36m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'
                    ;;
                *)
                    PS1='\[\033[01;34m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'
                    ;;
                esac
            ;;
    esac
fi

if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]
then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
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

# Maintain $HISTORY_FILE and if this is an xterm set the title to
# user@host:dir
case $TERM in
    uxterm*|xterm*|rxvt*)
        PROMPT_COMMAND="_append_history; history -a; _update_window_title"
        _configure_show_current_command_in_window_title
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

alias ll='ls -l'
alias la='ls -A'
alias l='ls -CFhla'

alias e=$EDITOR
alias em="emacs -nw -q -no-site-file"
alias E="EDITOR=\"emacsclient -c -a emacs\" sudoedit"

alias pru="ping google.com"
alias ws="wpa_cli status"
alias wr="wpa_cli reassociate"
alias ic="ip addr"
alias wpa_gui="/usr/sbin/wpa_gui"
alias wpa_cli="/sbin/wpa_cli"

alias a="acpi"
alias d="dict"

alias g="git"
alias gs="git status"
alias gc="git commit"
alias gb="git branch"
alias gl="git log"
alias gd="git diff"
alias less="less -j 5"

alias nexus-connect="dbus-send --system --type=method_call --dest=org.bluez --print-reply /org/bluez/hci0/dev_$NEXUS_MAC org.bluez.Network1.Connect string:nap"
alias nexus-disconnect="dbus-send --system --type=method_call --dest=org.bluez --print-reply /org/bluez/hci0/dev_$NEXUS_MAC org.bluez.Network1.Disconnect"

alias sound-connect="dbus-send --system --type=method_call --dest=org.bluez --print-reply /org/bluez/hci0/dev_$SOUND_MAC org.bluez.Device1.Connect"
alias sound-disconnect="dbus-send --system --type=method_call --dest=org.bluez --print-reply /org/bluez/hci0/dev_$SOUND_MAC org.bluez.Device1.Disconnect"

alias bton="dbus-send --system --type=method_call --dest=org.bluez --print-reply /org/bluez/hci0 org.freedesktop.DBus.Properties.Set string:org.bluez.Adapter1 string:Powered variant:boolean:true"
alias btoff="dbus-send --system --type=method_call --dest=org.bluez --print-reply /org/bluez/hci0 org.freedesktop.DBus.Properties.Set string:org.bluez.Adapter1 string:Powered variant:boolean:false"

if grep -i debian /etc/os-release &>/dev/null; then
    alias sa="sudo aptitude"
    alias acs="apt-cache search"
    alias acsh="apt-cache show"
    alias wu="sudo ifup wlan0"
    alias wd="sudo wpa_action wlan0 stop"
    alias eup="sudo ifup eth0=dhcp"
    alias edw="sudo ifdown eth0"
elif grep -i archlinux /etc/os-release &>/dev/null; then
    alias wu="sudo systemctl start netctl-auto@wlan0.service"
    alias wd="sudo systemctl stop netctl-auto@wlan0.service"
    alias eup="sudo systemctl start netctl@eth0.service"
    alias edw="sudo systemctl stop netctl@eth0.service"
    alias bup="sudo systemctl start netctl@bnep0.service"
    alias bdw="sudo systemctl stop netctl@bnep0.service"
fi

ff () {
    WHERE=${2-.}
    find $WHERE -name "$1"
}
rg () {
    WHERE=${2-.}
    grep -r "$1" $WHERE
}

export GPG_TTY=`tty`

yt () {
    mpv -cache 500 $(youtube-dl -g "$1")
}

if [ -f /etc/bash_completion.d/git ]; then
    . /etc/bash_completion.d/git
fi

# from technomancy
function fix-agent {
  SOCKETS=`find /tmp/ -uid $UID -path \*ssh\* -type s 2> /dev/null`
  export SSH_AUTH_SOCK=$(ls --color=never -t1 $SOCKETS | head -1)
  ssh-add -l
}

function demo_ps1 {
    PS1='\[\033[01;36m\]\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'
}

function tv-bfmtv {
    mpv mms://vipevenement.yacast.net/bfm_bfmtv
}

function tv-france24 {
    mpv mms://stream1.france24.yacast.net/f24_livefr
}

function radio-franceinter {
    mpv http://mp3.live.tv-radio.com/franceinter/all/franceinterhautdebit.mp3
}

function radio-franceinfo {
    mpv http://mp3.live.tv-radio.com/franceinfo/all/franceinfo.mp3
}

function radio-europe1 {
    mpv http://vipicecast.yacast.net/europe1
}

function radio-rfi {
    mpv http://mp3.live.tv-radio.com/rfimonde/all/rfimonde-64k.mp3
}

function radio-bfm {
    mpv http://vipicecast.yacast.net/bfm_web
}

function radio-francebleu {
    mpv http://mp3lg4.tdf-cdn.com/6841/fbazur-hd.mp3
}

function radio-rtl {
    mpv http://ais.rtl.fr:80/rtl-1-44-128
}

function radio-bbc4 {
    mpv http://bbcmedia.ic.llnwd.net/stream/bbcmedia_intl_lc_radio4_q
}

function radio-bbcws {
    mpv http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-eieuk
}

function radio-bbcwsn {
    mpv http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-einws
}

function ta {
    if [ "$TERM" != "screen-256color" ]
    then
        tmux attach-session -t "$USER" || tmux new-session -s "$USER"
        exit
    fi
}

function rsync_mirror {
    rsync -avzP --inplace -e 'ssh -o ClearAllForwardings=yes' "$@"
}

function server {
    local PORT="${1:-8000}"
    echo "http://localhost:$PORT"
    python -m SimpleHTTPServer $PORT
}

function hdmi {
    case "$1" in
        on)
            xrandr --output HDMI1 --auto
            pactl set-card-profile 0 output:hdmi-stereo
            ;;
        off)
            xrandr --output HDMI1 --off
            pactl set-card-profile 0 output:analog-stereo
            ;;
        *)
            echo "Usage: hdmi <on|off>"
            return 1
            ;;
    esac
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

fasd () {
    $_THIS_DIR/external/fasd/fasd "$@"
}
eval "$(fasd --init auto)"
