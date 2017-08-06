export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="kennethreitz"
HYPHEN_INSENSITIVE="true"

plugins=(cargo colorize cp docker extract gitfast github golang gulp npm pip rust sudo terraform vault virtualenv z zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

case $OSTYPE in
  darwin*)
    alias code="open /Applications/Emacs.app"
    ;;
  linux*)
    alias code="emacs &>/dev/null &!"
    alias agud="sudo apt update && sudo apt full-upgrade"
  ;;
esac

if type pew &> /dev/null; then
    source $(pew shell_config)
fi
