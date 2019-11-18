test -f ~/.environment && . ~/.environment
test -f ~/.environment-private && . ~/.environment-private

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s histappend
shopt -s checkwinsize

export MAILCHECK=-1
export HISTCONTROL=ignoreboth
export HISTSIZE=100000

PS1='\[\033[01;36m\]\u@\h:\[\033[01;33m\]\w\[\e[01;$(($??31:37))m\] \$ \[\033[00m\]'

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

PROMPT_COMMAND="_append_history; history -a; _update_window_title"

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

function copy_terminfo {
  local REMOTE_HOST=$1
  infocmp | ssh $REMOTE_HOST tic -
}

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
  nix-build -E 'with import <nixpkgs> {  }; haskellPackages.callPackage ./default.nix {  }'
}

function haskell-project-tags {
  nix-build --out-link dependencies --arg cabalProject ./. ~/src/me/haskell-sources-tags-nix/
  nix-shell -p haskellPackages.fast-tags --run "fast-tags -R ."
}
