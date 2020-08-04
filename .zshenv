# Reset PATH if this is a nested login shell.
# It will acquire fresh entries from zprofile.
if [[ $SHLVL > 1 && -o LOGIN ]]; then
    if [[ -s /etc/environment ]]; then
        source /etc/environment
        export PATH
    else
        export PATH=""
    fi
fi
