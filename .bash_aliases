#!/bin/bash
# User specific aliases and functions

LS_OPTIONS='--color=auto'

unalias -a
shopt -s expand_aliases

if command -v dircolors >/dev/null; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

alias ...='cd ../..'
alias ..='cd ..'
alias SUDO='sudo '
alias cp='cp -i'
alias dmesg='dmesg --color=auto'
alias e='setsid emacs 2>/dev/null'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias gdb='gdb -q'
alias grep='grep --color=auto'
alias ip='ip --color=auto'
alias l='ls -alF'
alias la='ls -la'
alias ll='ls -l'
alias ls='LC_COLLATE=C _ls --group-directories-first'
alias md='mkdir -p'
alias mv='mv -i'
alias py=python
alias python=python3
alias q='qalc -t -f -'
alias rd=rmdir
alias rm='rm -I'
alias th='gio trash'
alias tree='LC_COLLATE=C tree --dirsfirst'
alias v='emacs -nw'
alias vA='v ~/.bash_aliases && . ~/.bash_aliases; true'

_ls() { local IFS=' '; command ls $LS_OPTIONS ${1+"$@"} ; }
datefmt() { date +%y-%m-%d-%H-%M-%S ; }
fr() { cmd=$1 && shift || tree -L 1 ~/.var/app ; app="$(find ~/.var/app -maxdepth 1 -type d -iname "*$cmd*" -printf '%f\n' | grep -m1 .)" || return $? ; setsid flatpak run "$app" "$@" 2>/dev/null ; }
o() { setsid xdg-open "$@" >/dev/null 2>&1 ; }
stash() { git commit -a --allow-empty-message -m '' "$@" ; }
