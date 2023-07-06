if [[ -z "$CARGO_HOME" ]]; then
  export CARGO_HOME="$HOME/.cargo"
fi

path=("$CARGO_HOME/bin" $path)
typeset -U path
