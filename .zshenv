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

export NPM_CONFIG_USERCONFIG="$HOME/.config/npmrc"

if whence yarn > /dev/null; then
    export PATH="`yarn global bin`:$PATH"
fi

# Python

export PIPSI_HOME="$HOME/.local/share/virtualenvs"
export WORKON_HOME="$HOME/.local/share/virtualenvs"

# Ruby

if [ -d "/usr/local/share/chruby" ]; then
    source "/usr/local/share/chruby/chruby.sh"
    source "/usr/local/share/chruby/auto.sh"
fi

# Rust

if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

if [ -d "$HOME/.multirust" ]; then
    local rust_root=$(rustc --print sysroot)
    export RUST_SRC_PATH="$rust_root/lib/rustlib/src/rust/src"
fi
