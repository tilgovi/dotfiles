export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="kardan"
HYPHEN_INSENSITIVE="true"

plugins=(
    aws
    base16-shell
    cargo
    chruby
    colorize
    docker
    extract
    git
    gitfast
    golang
    heroku
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
    zsh-aws-vault
    zsh-syntax-highlighting
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

alias light=base16_solarized-light
alias dark=base16_solarized-dark
