#!/bin/bash
# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

# Disable Ctrl-s function
stty -ixon

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=200000

# Set GLOBIGNORE variable to hide the . and .. directories. This does
# automatically also set the dotglob option, so * now matches both hidden and
# non-hidden files.
GLOBIGNORE=.:..

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# ?(pattern-list)
#   Matches zero or one occurrence of the given patterns
# *(pattern-list)
#   Matches zero or more occurrences of the given patterns
# +(pattern-list)
#   Matches one or more occurrences of the given patterns
# @(pattern-list)
#   Matches one of the given patterns
# !(pattern-list)
#   Matches anything except one of the given patterns
shopt -s extglob

# Remove the need to specify cd command to change directory
shopt -s autocd

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# git-prompt.sh setup
unset gitfmt
for f in /etc/bash_completion.d/git-prompt.sh \
             "${PREFIX:-/usr}/share/git-core/contrib/completion/git-prompt.sh"
do
    if [ -f "$f" ]; then
        GIT_PS1_DESCRIBE_STYLE=branch
        GIT_PS1_HIDE_IF_PWD_IGNORED=1
        GIT_PS1_SHOWCOLORHINTS=1
        GIT_PS1_SHOWDIRTYSTATE=1
        GIT_PS1_SHOWSTASHSTATE=1
        GIT_PS1_SHOWUNTRACKEDFILES=1
        GIT_PS1_SHOWUPSTREAM=verbose
        gitfmt='$(__git_ps1 "\[\033[01;93m\](%s)\[\033[00m\]")'
        source "$f" && break
    fi
done
PROMPT_COMMAND='ec_fmt=$?; if [[ $ec_fmt == 0 ]];
                           then unset ec_fmt; else ec_fmt="($ec_fmt)"; fi'
PS1='\[\033[00m\]\[\033[01;94m\]\W\[\033[00m\]'
PS1+=$gitfmt'\[\033[01;31m\]$ec_fmt\[\033[00m\]\$ '
unset gitfmt

# Cursor style
printf '\x1b[\x35 q'

# Title
case ${TERM} in
    xterm*|rxvt*|gnome*|alacritty*|st-*)
        PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }$(cat <<EOF
        printf "\033]0;%s@%s:%s (%s)\007" "${USER}" "${HOSTNAME%%.*}" \
        "${PWD/#$HOME/\~}" "$0${@:+ $@}"
EOF
)
        ;;
    screen*)
        PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }$(cat <<EOF
        printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" \
        "${PWD/#$HOME/\~}"'
EOF
)
        ;;
esac

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f "${PREFIX:-/usr}/share/bash-completion/bash_completion" ]; then
        source "${PREFIX:-/usr}/share/bash-completion/bash_completion"
    elif [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    fi
fi

true
