set background=dark

set hlsearch
set ignorecase
set smartcase

set tabstop=4
set shiftwidth=4
set expandtab

set title
set scrolloff=7
set showcmd
set wildmode=longest,list
set number
set noswapfile
set modeline

set viminfo='1000,f1,<500
set undodir=~/.vimdid
set undofile

set statusline=%<%f\ %h%m%r%Y%=%-14.(%l,%c%V%)\ %P
set cursorline
autocmd InsertEnter,InsertLeave * set cursorline!

if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading
endif

colorscheme tokyonight-night
