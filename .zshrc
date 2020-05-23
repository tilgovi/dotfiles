export ZSH="$HOME/.oh-my-zsh"

ZSH_DISABLE_COMPFIX="true"
ZSH_THEME="starship"
ZSH_TMUX_AUTOSTART="true"
HYPHEN_INSENSITIVE="true"

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
        alias code="/Applications/Emacs.app/Contents/MacOS/Emacs &>/dev/null &!"
        ;;
    linux*)
        alias code="emacs &>/dev/null &!"
        alias agud="sudo apt update && sudo apt full-upgrade"
        ;;
esac

alias light=base16_atelier-forest-light
alias dark=base16_atelier-forest
