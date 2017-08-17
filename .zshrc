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

if test aws_zsh_completer.sh &> /dev/null; then
    source $(whence aws_zsh_completer.sh)
fi

if [ -d "/usr/local/share/chruby" ]; then
    source "/usr/local/share/chruby/chruby.sh"
    source "/usr/local/share/chruby/auto.sh"
fi

case $OSTYPE in
  darwin*)
    alias code="/Applications/Emacs.app/Contents/MacOS/Emacs &>/dev/null &!"
    ;;
  linux*)
    alias code="emacs &>/dev/null &!"
    alias agud="sudo apt update && sudo apt full-upgrade"
  ;;
esac
