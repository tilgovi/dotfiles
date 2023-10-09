if [[ -z "$RTX_DATA_DIR" ]]; then
  export RTX_DATA_DIR="${XDG_DATA_HOME:-${HOME}/.local/share}/rtx"
fi

path=("$RTX_DATA_DIR/shims" $path)
typeset -U path
