set encoding=utf-8

if (has("termguicolors"))
  set termguicolors
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

set background=light
" highlight Visual guibg=#444444
" highlight CursorLine cterm=NONE guibg=#222222
" highlight StatusLine cterm=bold guibg=#ffffff guifg=#000000
" highlight StatusLineNC cterm=NONE guibg=#666666 guifg=#000000
" highlight Search cterm=NONE guibg=#ffa7c4
" highlight QuickFixLine cterm=NONE guibg=#555555
" highlight NormalFloat cterm=NONE guibg=#333333

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
  buf_set_keymap('n', '<space>ws', '<cmd>lua vim.lsp.buf.workspace_symbol()<CR>', opts)
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
end

-- Use a loop to conveniently both setup defined servers
-- and map buffer local keybindings when the language server attaches
local servers = { "hls", "dhall_lsp_server", "purescriptls", "tsserver" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach, autostart = false }
end

metals_config = require("metals").bare_config
metals_config.init_options.statusBarProvider = "on"
metals_config.on_attach = on_attach

vim.g.metals_disabled_mode = true
vim.g.metals_use_global_executable = true

vim.cmd [[
augroup lsp
  au!
  au FileType scala,sbt lua require("metals").initialize_or_attach(metals_config)
augroup end
]]

require("nvim-treesitter.configs").setup{
  highlight = { enable = true },
  indent = { enable = true },
  playground = { enable = true },
}

-- Manually set the filetype (needed if vim-nix is not installed)
vim.api.nvim_command("autocmd BufNewFile,BufRead *.nix setlocal filetype=nix")
EOF
