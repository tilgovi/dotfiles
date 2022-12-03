typeset -U path
path=("$HOME/.local/bin" "/usr/local/bin" $path)

# Go
export GOMODCACHE=$HOME/.go

# Node
export NPM_CONFIG_CACHE="$HOME/.cache"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/npmrc"
export NVM_DIR="$HOME/.nvm"

# Python
path=("${PYENV_ROOT:-$HOME/.pyenv}/shims" $path)

# Rust
path=("$HOME/.cargo/bin" $path)

# Volta
export VOLTA_HOME="$HOME/.volta"
path=("$VOLTA_HOME/bin" $path)
