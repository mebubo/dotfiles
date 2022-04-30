# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s histappend
shopt -s checkwinsize

HISTCONTROL=ignoreboth
HISTSIZE=100000

export EDITOR=vim
export LANG=fr_FR.UTF-8
export LESS=-j10

PS1='\[\033[01;36m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'

HISTORY_FILE=~/history

_append_history () {
    local LAST_CMD=$(history 1)
    # strip useless entry number from the beginning
    LAST_CMD=${LAST_CMD#*  }
    echo "$(date) -- cd $(pwd); $LAST_CMD" >> $HISTORY_FILE
}

PROMPT_COMMAND="_append_history; history -a"
