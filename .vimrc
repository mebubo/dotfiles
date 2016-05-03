call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'pangloss/vim-javascript'
call plug#end()

set hlsearch
set ignorecase
set smartcase

set title

set t_Co=256
set background=dark

set scrolloff=7

if ! exists("mapleader")
  let mapleader = ","
endif
noremap ,, ,

nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>

nnoremap <leader>bp :bp<cr>
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bo <c-w>o

nnoremap <silent> <Leader><space> :CtrlP<CR>
nnoremap <leader>b<space> :CtrlPBuffer<cr>

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif

for prefix in ['i', 'n', 'v']
  for key in ['<Up>', '<Down>', '<Left>', '<Right>']
    exe prefix . "noremap " . key . " <Nop>"
  endfor
endfor
