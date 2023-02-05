set modeline
set modelines=5

set viminfo='100,<0,s10,h

filetype plugin indent on
syntax enable

if exists('$BASE16_THEME')
    \ && (!exists('g:colors_name')
    \ || g:colors_name != 'base16-$BASE16_THEME')
  let base16colorspace=256
  colorscheme base16-$BASE16_THEME
endif
