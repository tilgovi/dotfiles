export ZSH="$HOME/.oh-my-zsh"

HYPHEN_INSENSITIVE="true"
ZSH_THEME="starship"
ZSH_TMUX_AUTOSTART="true"

plugins=(
    aws
    base16-shell
    cargo
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
    nvm
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
        ;;
    linux*)
        alias agud="sudo apt update && sudo apt full-upgrade"
        ;;
esac

alias code="emacs &>/dev/null &!"

alias light=base16_atelier-forest-light
alias dark=base16_atelier-forest
