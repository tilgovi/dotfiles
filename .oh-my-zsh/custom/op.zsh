OP_PLUGINS="${OP_CONFIG_DIR:-${XDG_CONFIG_HOME:-$HOME/.config/op}}/plugins.sh"

if [[ -f "${OP_PLUGINS}" ]]; then
  source "${OP_PLUGINS}"
fi

unset OP_PLUGINS
