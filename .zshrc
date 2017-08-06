export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="gentoo"
HYPHEN_INSENSITIVE="true"

plugins=(
    cargo
    colorize
    docker
    extract
    gitfast
    golang
    pip
    python
    ruby
    rust
    sudo
    terraform
    vault
    yarn
    z
    zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# User configuration

if [ -d "/usr/local/share/chruby" ]; then
    source "/usr/local/share/chruby/chruby.sh"
    source "/usr/local/share/chruby/auto.sh"
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
