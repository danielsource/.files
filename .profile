#!/bin/sh

new_path() {
    case ":$PATH:" in
    *:"$1":*) ;;
    *)
        if [ -d "$1" ]; then
            PATH=${PATH:+$2}
        fi
    esac
}
append_path () { new_path "$1" "$PATH:$1"; }
prepend_path () { new_path "$1" "$1:$PATH"; }

# Default XDG directories
export \
    XDG_CACHE_HOME="$HOME/.cache" \
    XDG_CONFIG_HOME="$HOME/.config" \
    XDG_DATA_HOME="$HOME/.local/share"

# Preferences
export \
    ANDROID_HOME="$XDG_DATA_HOME/android-home/sdk" \
    CARGO_HOME="$XDG_DATA_HOME/cargo" \
    EDITOR="emacs" \
    FZF_DEFAULT_COMMAND='find .' \
    FZF_DEFAULT_OPTS='--layout=reverse --no-color' \
    GOPATH="$XDG_DATA_HOME/go" \
    LESS='-MR --mouse' \
    LESSHISTFILE=- \
    MOZ_USE_XINPUT2=1 \
    NPM_PACKAGES="$XDG_DATA_HOME/npm" \
    SUDO_EDITOR="vim"

append_path "$ANDROID_HOME/emulator"
append_path "$ANDROID_HOME/platform-tools"
append_path "$GOPATH/bin"
append_path "$HOME/.local/bin"
append_path "$NPM_PACKAGES/bin"
prepend_path "$HOME/bin"

MANPATH="$NPM_PACKAGES/share/man:${MANPATH:-$(manpath)}"
export MANPATH

NODE_PATH="$NPM_PACKAGES/lib/node_modules:${PREFIX:-/usr}/lib/node_modules"
export NODE_PATH

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f ~/.bashrc ]; then
        . ~/.bashrc
    fi
fi
