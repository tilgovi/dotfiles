export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="kennethreitz"
HYPHEN_INSENSITIVE="true"

plugins=(cargo colorize cp docker extract gitfast github golang gulp npm pip rust sudo terraform vault virtualenv z zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

export EDITOR="vim"
export GOPATH="$HOME/src/golang"
export PATH="$HOME/.local/bin:$GOPATH/bin:$PATH"

export SSH_ASKPASS="ssh-askpass"

export URBIT_HOME=$HOME/src/urbit/urb
export WORKON_HOME="$HOME/.virtualenvs"

export npm_config_userconfig="$HOME/.config/npmrc"

if [ -d "/usr/local/share/chruby" ]; then
  source "/usr/local/share/chruby/chruby.sh"
  source "/usr/local/share/chruby/auto.sh"
fi

if [ -f "$HOME/.cargo/env" ]; then
  source "$HOME/.cargo/env"
fi

if [ -d "$HOME/.multirust" ]; then
  local rust_root=$(rustc --print sysroot)
  export RUST_SRC_PATH="$rust_root/lib/rustlib/src/rust/src"
fi

if whence yarn > /dev/null; then
  export PATH="`yarn global bin`:$PATH"
fi

case $OSTYPE in
  darwin*)
    alias code="open /Applications/Emacs.app"
    ;;
  linux*)
    alias code="emacs &>/dev/null &!"
    alias agud="sudo apt update && sudo apt full-upgrade"
  ;;
esac
