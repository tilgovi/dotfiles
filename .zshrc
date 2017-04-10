# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="powerlevel9k/powerlevel9k"

HYPHEN_INSENSITIVE="true"

plugins=(aws cargo chruby colorize command-not-found common-aliases cp docker extract gitfast github golang gulp httpie npm pip rust sudo terraform vault z zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

POWERLEVEL9K_COLOR_SCHEME='light'
POWERLEVEL9K_MODE="awesome-fontconfig"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(background_jobs dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(virtualenv)

export EDITOR="vim"
export GOPATH="$HOME/src/golang"
export PATH="$HOME/.local/bin:$GOPATH/bin:$PATH"

export SSH_ASKPASS="ssh-askpass"

export URBIT_HOME=$HOME/src/urbit/urb
export WORKON_HOME="$HOME/.virtualenvs"

export npm_config_userconfig="$HOME/.config/npmrc"

if [ -f "$HOME/.cargo/env" ]; then
  source "$HOME/.cargo/env"
fi

if [ -d "$HOME/.multirust" ]; then
  local rust_root=$(rustc --print sysroot)
  export RUST_SRC_PATH="$rust_root/lib/rustlib/src/rust/src"
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
