if [[ -z "$MISE_DATA_DIR" ]]; then
  export MISE_DATA_DIR="${XDG_DATA_HOME:-${HOME}/.local/share}/mise"
fi

path=("$MISE_DATA_DIR/shims" $path)
typeset -U path

export MISE_NODE_COREPACK=true
export MISE_NODE_DEFAULT_PACKAGES_FILE="${XDG_CONFIG_HOME:-${HOME}/.config}/default-npm-packages"
