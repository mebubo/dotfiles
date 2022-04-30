# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR=vim
export LANG=fr_FR.UTF-8
export LESS="-r -X -j10"

shopt -s histappend
shopt -s checkwinsize

HISTCONTROL=ignoreboth
HISTSIZE=100000

PS1='\[\033[01;36m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'

_append_history () {
    local HISTORY_FILE=~/.history
    local LAST_CMD=$(history 1 | sed 's/^ *[0-9]* *//')
    echo "$(date) -- cd $PWD && $LAST_CMD" >> $HISTORY_FILE
    history -a
}

PROMPT_COMMAND="_append_history"
