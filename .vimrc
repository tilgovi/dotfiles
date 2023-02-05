set modeline
set modelines=5

set viminfo='100,<0,s10,h

filetype plugin indent on
syntax enable

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif
