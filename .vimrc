filetype plugin indent on
syntax enable

set autoindent
set autoread
set backspace=indent,eol,start
set complete-=i
set display+=lastline
set formatoptions+=j " Delete comment character when joining commented lines
set ft=vim et sw=2
set history=2000
set incsearch
set mouse=a
set nrformats-=octal
set ruler
set smarttab
set wildmenu

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^Eterm'
  set t_Co=16
endif
