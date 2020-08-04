# Shell

export PATH="$HOME/.local/bin:$PATH"

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

export NPM_CONFIG_CACHE="$HOME/.cache"
export NPM_CONFIG_USERCONFIG="$HOME/.config/npmrc"
export NVM_DIR="$HOME/.nvm"
if [[ -s "$NVM_DIR/nvm.sh" ]]; then
    source "$NVM_DIR/nvm.sh"
else
    export N_PREFIX="$HOME/.local"
    export N_PRESERVE_NPM=1
    export NPM_CONFIG_PREFIX="$HOME/.local"
fi

# Python

export PIPSI_HOME="$HOME/.local/share/virtualenvs"
export WORKON_HOME="$HOME/.local/share/virtualenvs"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

# Rust
export PATH="$HOME/.cargo/bin:$PATH"
