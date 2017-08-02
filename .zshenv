export EDITOR="vim"
export GOPATH="$HOME/src/golang"
export PATH="$HOME/.local/bin:$GOPATH/bin:$PATH"

export SSH_ASKPASS="ssh-askpass"

export URBIT_HOME=$HOME/src/urbit/urb

export npm_config_userconfig="$HOME/.config/npmrc"

if [ -d "/usr/local/share/chruby" ]; then
    source "/usr/local/share/chruby/chruby.sh"
    source "/usr/local/share/chruby/auto.sh"
fi

if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

if [ -d "$HOME/.multirust" ]; then
    local rust_root=$(rustc --print sysroot)
    export RUST_SRC_PATH="$rust_root/lib/rustlib/src/rust/src"
fi

if whence yarn > /dev/null; then
    export PATH="`yarn global bin`:$PATH"
fi
