typeset -U path
path+="$HOME/.local/bin"

# Go
export GOMODCACHE=$HOME/.go

# Node
export NPM_CONFIG_CACHE="$HOME/.cache"
export NPM_CONFIG_PREFIX="$HOME/.local"
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/npmrc"

# Python
path+="${PYENV_ROOT:-$HOME/.pyenv}/shims"

# Rust
path+="$HOME/.cargo/bin"

# Volta
export VOLTA_HOME="$HOME/.volta"
path+="$VOLTA_HOME/bin"
