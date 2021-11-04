set modeline
set modelines=5

set viminfo='100,<0,s10,h

filetype plugin indent on
syntax enable

let g:nd_themes = [
  \ [ 'sunrise+0', 'base16-tomorrow', 'light' ],
  \ [ 'sunset+0',  'base16-tomorrow-night', 'dark'  ],
  \ ]
let g:nd_latitude = 40

if strftime("%m") > 2 && strftime("%m") < 10
  let g:nd_timeshift = 69
else
  let g:nd_timeshift = 9
endif

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif
