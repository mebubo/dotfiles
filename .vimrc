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
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'jiangmiao/auto-pairs'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'rking/ag.vim'
Plug 'leafgarland/typescript-vim'
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

set cursorline
highlight CursorLine cterm=NONE ctermbg=234
nnoremap <Leader>c :set cursorline!<CR>
autocmd InsertEnter,InsertLeave * set cursorline!

set statusline=%<%f\ %h%m%r%Y%=%-14.(%l,%c%V%)\ %P
highlight StatusLine ctermfg=7
autocmd InsertEnter * highlight StatusLine ctermfg=5
autocmd InsertLeave * highlight StatusLine ctermfg=7

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

autocmd BufWritePre * :%s/\s\+$//e

let g:ag_working_path_mode="r"
nnoremap K :Ag "\b<C-R><C-W>\b"<CR><CR>

set tags+=codex.tags;/
