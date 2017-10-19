let g:plug_shallow = 0

call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-eunuch'

Plug 'jiangmiao/auto-pairs'
Plug 'michaeljsmith/vim-indent-object', {'commit': '41d700f14b3decccdde421fbfe49e95a084a2f89'}
Plug 'scrooloose/syntastic'

Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'mileszs/ack.vim'
Plug 'mhinz/vim-grepper', {'commit': 'ab3c715c1507c2f4386cbabe9966ccdcf2858754'}

Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'leafgarland/typescript-vim'
Plug 'purescript-contrib/purescript-vim'
Plug 'FrigoEU/psc-ide-vim'
call plug#end()

set hlsearch
set ignorecase
set smartcase

set tabstop=4
set shiftwidth=4
set expandtab

autocmd FileType javascript setlocal shiftwidth=2 tabstop=2

set title

set t_Co=256
set background=dark

set scrolloff=7

set showcmd

set wildmode=longest,list

set number

set noswapfile

set modeline

let mapleader = ","
noremap ,, ,

nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>

nnoremap <leader>f :CtrlP<CR>
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>m :CtrlPMixed<CR>

nnoremap <leader>w :w<CR>
inoremap jk <Esc>

nnoremap <leader>t :NERDTreeToggle<CR>
nnoremap <leader>T :NERDTreeFind<CR>

set cursorline
highlight CursorLine cterm=NONE ctermbg=234
nnoremap <Leader>c :set cursorline!<CR>
autocmd InsertEnter,InsertLeave * set cursorline!

set statusline=%<%f\ %h%m%r%Y%=%-14.(%l,%c%V%)\ %P
highlight StatusLine ctermfg=7
autocmd InsertEnter * highlight StatusLine ctermfg=5
autocmd InsertLeave * highlight StatusLine ctermfg=7

if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading
  let g:ackprg = 'rg --vimgrep --no-heading'
  let g:ctrlp_user_command = 'rg --files %s'
  let g:ctrlp_use_caching = 0
elseif executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ackprg = 'ag --vimgrep'
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif

for prefix in ['i', 'n', 'v']
  for key in ['<Up>', '<Down>', '<Left>', '<Right>']
    exe prefix . "noremap " . key . " <Nop>"
  endfor
endfor

autocmd BufWritePre * :%s/\s\+$//e

nnoremap K :Ack "\b<C-R><C-W>\b"<CR><CR>

set tags+=codex.tags;/

let g:syntastic_scala_checkers=['']
let g:psc_ide_syntastic_mode = 1
