# Reset some variables if this is a login shell.
# These will acquire fresh values from zprofile.
if [[ -o LOGIN ]]; then
  export PATH=""
  export MAN_PATH=""
fi
