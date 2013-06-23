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
                mebub|britany)
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

# Maintain $HISTORY_FILE and if this is an xterm set the title to
# user@host:dir
case $TERM in
    uxterm*|xterm*|rxvt*)
	    PROMPT_COMMAND="_append_history; history -a; _update_window_title"
	    ;;
    *)
	    PROMPT_COMMAND="_append_history; history -a"
	    ;;
esac

case $(tty) in
    /dev/tty*)
        TMOUT=300
        ;;
esac

alias ll='ls -l'
alias la='ls -A'
alias l='ls -CFhla'
alias acs="apt-cache search"
alias acsh="apt-cache show"
alias acp="apt-cache policy"
alias agi="sudo aptitude install"
alias pru="ping ya.ru"
alias e=$EDITOR
alias em="emacs -nw -q -no-site-file"
alias sa="sudo aptitude"
alias wd="sudo wpa_action wlan0 stop"
alias wu="sudo ifup wlan0"
alias wl="less /var/log/wpa_action.wlan0.log"
alias wt="tail /var/log/wpa_action.wlan0.log"
alias ws="wpa_cli status"
alias wr="wpa_cli reassociate"
alias eup="sudo ifup eth0=dhcp"
alias edw="sudo ifdown eth0"
alias ic="/sbin/ifconfig"
alias wpa_gui="/usr/sbin/wpa_gui"
alias wpa_cli="/sbin/wpa_cli"
alias a="acpi"
alias d="dict"
alias E="EDITOR=\"emacsclient -c -a emacs\" sudoedit"
alias g="git"
alias gs="git status"
alias gc="git commit"
alias gb="git branch"
alias gl="git log"
alias gd="git diff"

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
    mplayer -cache 500 $(youtube-dl -g "$1")
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

function demo_commit_change {
    echo "DEMO Change $1" >> RM-README.txt
    git add RM-README.txt
    git commit -m "DEMO Change $1"
}

function bfmtv {
    mplayer mms://vipevenement.yacast.net/bfm_bfmtv
}

function france24 {
    mplayer mms://stream1.france24.yacast.net/f24_livefr
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
    echo "http://locahost:$PORT"
    python -m SimpleHTTPServer $PORT
}
