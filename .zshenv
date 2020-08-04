# Shell

export PATH="$HOME/.local/bin:$PATH"
export MAN_PATH="$HOME/.local/share/man:$MAN_PATH"

# AWS

case $OSTYPE in
    linux*)
        export AWS_VAULT_BACKEND="kwallet"
        ;;
esac

# Golang

export GOPATH="$HOME/src/golang"
export PATH="$GOPATH/bin:$PATH"

# Node

export N_PREFIX="$HOME/.local"
export N_PRESERVE_NPM=1
export NPM_CONFIG_CACHE="$HOME/.cache"
export NPM_CONFIG_USERCONFIG="$HOME/.config/npmrc"

# Python

export PIPSI_HOME="$HOME/.local/share/virtualenvs"
export WORKON_HOME="$HOME/.local/share/virtualenvs"

# Rust
export PATH="$HOME/.cargo/bin:$PATH"
