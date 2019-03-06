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

Plug 'jiangmiao/auto-pairs', {'commit': 'f0019fc6423e7ce7bbd01d196a7e027077687fda'}
Plug 'michaeljsmith/vim-indent-object', {'commit': '5c5b24c959478929b54a9e831a8e2e651a465965'}
Plug 'justinmk/vim-sneak', {'commit': '91192d8969af1d86c98beff4001612976ea25d7a'}
Plug 'ctrlpvim/ctrlp.vim', {'commit': '5e40e555d31d9cce2188d9fa724d1debcad28aa6'}
Plug 'ivalkeen/vim-ctrlp-tjump', {'commit': '830a409f7e6f19a8632e33591ae10bef32329158'}
Plug 'scrooloose/nerdtree', {'commit': '91e0f2253fbecefa7e14f095950341584877ef19'}
Plug 'mileszs/ack.vim', {'commit': '36e40f9ec91bdbf6f1adf408522a73a6925c3042'}
Plug 'mhinz/vim-grepper', {'commit': '4a47e20c98eee758b905a2cd7ca29f433c08e7e7'}

Plug 'LnL7/vim-nix', {'commit': 'be0c6bb409732b79cc86c177ca378b0b334e1efe'}
Plug 'purescript-contrib/purescript-vim', {'commit': '67ca4dc4a0291e5d8c8da48bffc0f3d2c9739e7f'}
Plug 'FrigoEU/psc-ide-vim', {'commit': '6d4a3cc27e9782b703f6dd61ef5fdf27054bac0f'}

Plug 'derekwyatt/vim-scala', {'commit': '971ac9ab3fe945105ef88587cfe5273fa2c8e988'}
Plug 'natebosch/vim-lsc', {'commit': 'f7adfe36af3afbd557c885819b6218a256f375ea'}
call plug#end()

set encoding=utf-8

if (has("termguicolors"))
  set termguicolors
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

set background=dark
highlight Visual guibg=#444444
highlight CursorLine cterm=NONE guibg=#222222
highlight StatusLine cterm=bold guibg=#ffffff guifg=#000000
highlight StatusLineNC cterm=NONE guibg=#666666 guifg=#000000

set hlsearch
set ignorecase
set smartcase

set tabstop=4
set shiftwidth=4
set expandtab

autocmd FileType javascript setlocal shiftwidth=2 tabstop=2
autocmd FileType yaml setlocal shiftwidth=2 tabstop=2
autocmd FileType python setlocal shiftwidth=4 tabstop=4

set title
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
nnoremap <leader>t :NERDTreeToggle<CR>
nnoremap <leader>T :NERDTreeFind<CR>

set cursorline
nnoremap <Leader>c :set cursorline!<CR>
autocmd InsertEnter,InsertLeave * set cursorline!

set statusline=%<%f\ %h%m%r%Y%=%-14.(%l,%c%V%)\ %P

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

nnoremap K :Ack! "\b<C-R><C-W>\b"<CR>
nnoremap <Leader>a :Ack!<Space>

set tags+=codex.tags;/

nnoremap <c-]> :CtrlPtjump<cr>
vnoremap <c-]> :CtrlPtjumpVisual<cr>

let g:ctrlp_tjump_shortener = ['/home/[^/]*/.stack/indices/Hackage/packages/', '~~/']
let g:ctrlp_tjump_only_silent = 1
let g:ctrlp_tjump_skip_tag_name = 1

let g:NERDTreeWinSize = 40

if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

imap <F3> <C-R>=strftime("%Y-%m-%d")<CR>

au BufRead,BufNewFile *.sbt set filetype=scala

let g:lsc_enable_autocomplete = v:false
let g:lsc_server_commands = {
  \  'scala': {
  \    'command': '/home/me/bin/metals-vim',
  \    'log_level': 'Log'
  \  }
  \}
let g:lsc_auto_map = {
  \  'GoToDefinition': 'gd',
  \}
