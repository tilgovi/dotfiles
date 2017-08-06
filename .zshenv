# Shell

export PATH="$HOME/.local/bin:$PATH"

# AWS

case $OSTYPE in
    linux*)
        export AWS_VAULT_BACKEND="kwallet"
        ;;
esac

# Golang

export GOPATH="$HOME/src/golang"
export PATH="$GOPATH/bin:$PATH"

# Node

export NPM_CONFIG_USERCONFIG="$HOME/.config/npmrc"

if whence yarn > /dev/null; then
    export PATH="`yarn global bin`:$PATH"
fi

# Python

export PIPSI_HOME="$HOME/.local/share/virtualenvs"
export WORKON_HOME="$HOME/.local/share/virtualenvs"
