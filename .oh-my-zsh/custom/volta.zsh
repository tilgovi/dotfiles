if [[ -z "$VOLTA_HOME" ]]; then
  export VOLTA_HOME="$HOME/.volta"
fi

path=("$VOLTA_HOME/bin" $path)
typeset -U path
