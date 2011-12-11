# ~/.bashrc: executed by bash(1) for non-login shells.

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
            PS1='\[\033[01;36m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'
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

my_hist_file=history
my_hist_comm='echo `date` " -- cd" `pwd`";" `history 1` >> ~/${my_hist_file}'

# Maintain ~/${my_hist_file} and if this is an xterm set the title to
# user@host:dir
case $TERM in
    uxterm*|xterm*|rxvt*)
	    PROMPT_COMMAND="${my_hist_comm}; history -a; "'echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
	    ;;
    *)
	    PROMPT_COMMAND="${my_hist_comm}; history -a;"
	    ;;
esac

. ~/.environment

alias ll='ls -l'
alias la='ls -A'
alias l='ls -CFhla'
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
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

export GPG_TTY=`tty`

yt () {
    mplayer -cache 500 $(youtube-dl -g "$1")
}

if [ -f /etc/bash_completion.d/git ]; then
    . /etc/bash_completion.d/git
fi
