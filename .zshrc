export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="kardan"
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
