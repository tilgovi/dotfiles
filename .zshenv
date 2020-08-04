# Reset some variables if this is a nested login shell.
# These will acquire fresh values from zprofile.
if [[ $SHLVL > 1 && -o LOGIN ]]; then
  export PATH="/usr/local/bin:/usr/bin/:/bin:/usr/games"
  export MAN_PATH=""
fi
