export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="gentoo"
HYPHEN_INSENSITIVE="true"

plugins=(
    aws
    cargo
    chruby
    colorize
    docker
    extract
    gitfast
    golang
    pip
    pyenv
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

case $OSTYPE in
    darwin*)
        alias code="/Applications/Emacs.app/Contents/MacOS/Emacs &>/dev/null &!"
        ;;
    linux*)
        alias code="emacs &>/dev/null &!"
        alias agud="sudo apt update && sudo apt full-upgrade"
        ;;
esac
