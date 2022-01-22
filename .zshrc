export ZSH="$HOME/.oh-my-zsh"

HYPHEN_INSENSITIVE="true"
ZSH_THEME="starship"
ZSH_TMUX_AUTOSTART="true"

plugins=(
    aws
    base16-shell
    chruby
    colorize
    command-not-found
    docker
    extract
    gcloud
    git
    gitfast
    golang
    heroku
    kubectl
    pip
    pyenv
    python
    ruby
    rust
    sudo
    terraform
    tmux
    vault
    yarn
    z
    zsh-autosuggestions
    zsh-aws-vault
)

source $ZSH/oh-my-zsh.sh

# User configuration

prompt_aws_vault() {
    local vault_segment
    vault_segment="`prompt_aws_vault_segment`"
    [[ $vault_segment != '' ]] && PROMPT="$vault_segment $PROMPT"
}

prompt_aws_vault

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

alias light=base16_tomorrow
alias dark=base16_tomorrow-night
