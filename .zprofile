# AWS

case $OSTYPE in
    linux*)
        export AWS_VAULT_BACKEND="kwallet"
        ;;
esac

# Go

export GOMODCACHE=$HOME/.go

# Node

export NPM_CONFIG_CACHE="$HOME/.cache"
export NPM_CONFIG_PREFIX="$HOME/.local"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/npmrc"
export VOLTA_HOME="$HOME/.volta"
export PATH="$HOME/.volta/bin:$PATH"

# OCaml

if [[ -r "$HOME/.opam/opam-init/init.zsh" ]]; then
    source "$HOME/.opam/opam-init/init.zsh"
fi

# Python

export PIPSI_HOME="$HOME/.local/share/virtualenvs"
export WORKON_HOME="$HOME/.local/share/virtualenvs"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init --path)"
fi

# Rust

export PATH="$HOME/.cargo/bin:$PATH"

# Shell

export PATH="$HOME/.local/bin:$PATH"
