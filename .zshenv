typeset -U path
path=("$HOME/.local/bin" "/usr/local/bin" $path)

# Go
export GOMODCACHE=$HOME/.go

# Node
export NPM_CONFIG_CACHE="$HOME/.cache"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/npmrc"
export NVM_DIR="$HOME/.nvm"

# Python
export PYENV_ROOT=$HOME/.pyenv
path=("$PYENV_ROOT/shims" $path)

# Rust
export CARGO_HOME=$HOME/.cargo
path=("$CARGO_HOME/bin" $path)

# Volta
export VOLTA_HOME="$HOME/.volta"
path=("$VOLTA_HOME/bin" $path)
