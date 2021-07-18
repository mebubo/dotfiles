if exists('plug#begin')

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
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-obsession'

Plug 'jiangmiao/auto-pairs', {'commit': 'f0019fc6423e7ce7bbd01d196a7e027077687fda'}
Plug 'michaeljsmith/vim-indent-object', {'commit': '5c5b24c959478929b54a9e831a8e2e651a465965'}
Plug 'justinmk/vim-sneak', {'commit': '91192d8969af1d86c98beff4001612976ea25d7a'}
Plug 'ctrlpvim/ctrlp.vim', {'commit': '5e40e555d31d9cce2188d9fa724d1debcad28aa6'}
Plug 'ivalkeen/vim-ctrlp-tjump', {'commit': '154b5dc7b4651c64a25a4914972b3a5231d5c9d8'}
Plug 'scrooloose/nerdtree', {'commit': '91e0f2253fbecefa7e14f095950341584877ef19'}
Plug 'mileszs/ack.vim', {'commit': '36e40f9ec91bdbf6f1adf408522a73a6925c3042'}
Plug 'mhinz/vim-grepper', {'commit': '4a47e20c98eee758b905a2cd7ca29f433c08e7e7'}

Plug 'LnL7/vim-nix', {'commit': 'be0c6bb409732b79cc86c177ca378b0b334e1efe'}
Plug 'purescript-contrib/purescript-vim', {'commit': '67ca4dc4a0291e5d8c8da48bffc0f3d2c9739e7f'}
Plug 'FrigoEU/psc-ide-vim', {'commit': '6d4a3cc27e9782b703f6dd61ef5fdf27054bac0f'}

Plug 'derekwyatt/vim-scala', {'commit': '971ac9ab3fe945105ef88587cfe5273fa2c8e988'}
call plug#end()

endif

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
nnoremap <leader>f :Tags<CR>
nnoremap <leader>r :Tags '<c-r><c-w><CR>
" nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <c-p> :Files<CR>
nnoremap <leader>m :CtrlPMixed<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>t :NERDTreeToggle<CR>
nnoremap <leader>T :NERDTreeFind<CR>

set cursorline
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

set tags+=dependencies/tags

" nnoremap <c-]> :CtrlPtjump<cr>
" vnoremap <c-]> :CtrlPtjumpVisual<cr>
nnoremap <c-]> :Tags '<c-r><c-w><cr>

let g:ctrlp_tjump_shortener = ['^\(dependencies/\|/nix/store/.*-haskell-sources-tags/\)', '.../']
let g:ctrlp_tjump_only_silent = 1
let g:ctrlp_tjump_skip_tag_name = 1

let g:NERDTreeWinSize = 40

autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

imap <F3> <C-R>=strftime("%Y-%m-%d")<CR>

autocmd BufRead,BufNewFile *.sbt set filetype=scala
autocmd FileType sbt setlocal commentstring=//\ %s

nnoremap <leader>l :ls<CR>:b<space>

set viminfo='1000,f1,<500

set undodir=~/.vimdid
set undofile

lua << EOF
local nvim_lsp = require('lspconfig')
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)

  -- Set some keybinds conditional on server capabilities
  if client.resolved_capabilities.document_formatting then
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  elseif client.resolved_capabilities.document_range_formatting then
    buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  end

  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
      hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
      hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end
end

-- Use a loop to conveniently both setup defined servers
-- and map buffer local keybindings when the language server attaches
local servers = { "hls", "dhall_lsp_server" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach }
end

metals_config = require("metals").bare_config
metals_config.init_options.statusBarProvider = "on"

vim.g.metals_disabled_mode = true
vim.g.metals_use_global_executable = true

vim.cmd [[
augroup lsp
  au!
  au FileType scala,sbt lua require("metals").initialize_or_attach(metals_config)
augroup end
]]

require("nvim-treesitter.configs").setup{
  highlight={enable=true},
  indent={enable=true},
}

-- Manually set the filetype (needed if vim-nix is not installed)
vim.api.nvim_command("autocmd BufNewFile,BufRead *.nix setlocal filetype=nix")
EOF
