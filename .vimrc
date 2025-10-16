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
highlight Search cterm=NONE guibg=#ffa7c4
highlight QuickFixLine cterm=NONE guibg=#555555
highlight NormalFloat cterm=NONE guibg=#333333

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

set mouse=

set viminfo='1000,f1,<500
set undodir=~/.vimdid
set undofile

set tags+=dependencies/tags

set statusline=%<%f\ %h%m%r%Y%=%-14.(%l,%c%V%)\ %P
set cursorline
autocmd InsertEnter,InsertLeave * set cursorline!

autocmd FileType nix setlocal commentstring=#\ %s
autocmd FileType sbt setlocal commentstring=//\ %s
autocmd BufRead,BufNewFile *.sbt set filetype=scala
autocmd BufWritePre * :%s/\s\+$//e

let mapleader = ","
noremap ,, ,

imap <F3> <C-R>=strftime("%Y-%m-%d")<CR>
nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>l :ls<CR>:b<space>

nnoremap <leader>ft :Tags<CR>
nnoremap <leader>fr :Tags '<c-r><c-w><CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fh :History<CR>
" nnoremap <c-p> :Files<CR>
nnoremap <c-]> :Tags '<c-r><c-w><CR>

nnoremap <leader><space> :Telescope builtin<CR>
nnoremap <leader>tt :Telescope tags<CR>
nnoremap <leader>tr :lua require'telescope.builtin'.tags { default_text = vim.fn.expand("<cword>") }<CR>
nnoremap <leader>tb :Telescope buffers<CR>
nnoremap <leader>tf :Telescope find_files follow=true<CR>
nnoremap <leader>th :Telescope oldfiles<CR>
nnoremap <c-p> :Telescope find_files follow=true<CR>
nnoremap <c-b> :Telescope buffers<CR>
nnoremap <leader>b :Telescope buffers<CR>
" nnoremap <c-]> :lua require'telescope.builtin'.tags { default_text = vim.fn.expand("<cword>") }<CR>

nnoremap <leader>nt :NERDTreeToggle<CR>
nnoremap <leader>nT :NERDTreeFind<CR>

nnoremap K :Ack! "\b<C-R><C-W>\b"<CR>
nnoremap <Leader>a :Ack!<Space>

let g:NERDTreeWinSize = 40

if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-heading
  let g:ackprg = 'rg --vimgrep --no-heading'
elseif executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ackprg = 'ag --vimgrep'
endif

lua << EOF

require("nvim-treesitter.configs").setup{
  highlight = { enable = true },
  indent = { enable = true },
  playground = { enable = true },
}

-- Manually set the filetype (needed if vim-nix is not installed)
vim.api.nvim_command("autocmd BufNewFile,BufRead *.nix setlocal filetype=nix")

require('telescope').setup{
  pickers = {
    buffers = {
      sort_lastused = true
    }
  }
}
EOF
