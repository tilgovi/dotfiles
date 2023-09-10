export ZSH="$HOME/.oh-my-zsh"

HYPHEN_INSENSITIVE="true"
ZSH_PYENV_QUIET="true"

zstyle ':omz:plugins:nvm' lazy yes
zstyle ':omz:update' mode reminder

plugins=(
    1password
    aws
    base16-shell
    chruby
    docker
    extract
    gcloud
    git
    golang
    kubectl
    nvm
    pip
    pyenv
    rust
    starship
    terraform
    vault
    volta
    yarn
    z
    zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

# User configuration

case $OSTYPE in
    darwin*)
        if [[ ${+commands[gdircolors]} == 1 ]]; then
            eval $(gdircolors)
            alias ls="gls --color=tty"
        fi
        ;;
    linux*)
        alias agud="sudo apt update && sudo apt full-upgrade"
        ;;
esac

alias code="emacs &>/dev/null &!"

alias light=base16_selenized-white
alias dark=base16_selenized-black
