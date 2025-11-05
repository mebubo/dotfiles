vim.opt.mouse = ''

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.keymap.set('i', '<F3>', function()
  return vim.fn.strftime('%Y-%m-%d')
end, { expr = true, silent = true, desc = 'Insert date' })

vim.keymap.set('n', '<leader>e', function()
  local dir = vim.fn.expand('%:p:h') .. '/'
  vim.api.nvim_feedkeys(':edit ' .. dir, 'n', false)
end, { silent = true, desc = 'Edit current file dir' })

vim.keymap.set('n', '<leader>w', ':write<CR>')
vim.keymap.set('n', '<leader>l', ':ls<CR>:b ')

vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})

vim.api.nvim_create_autocmd('BufRead', {
  callback = function(opts)
    vim.api.nvim_create_autocmd('BufWinEnter', {
      once = true,
      buffer = opts.buf,
      callback = function()
        local ft = vim.bo[opts.buf].filetype
        local last_known_line = vim.api.nvim_buf_get_mark(opts.buf, '"')[1]
        if
          not (ft:match('commit') and ft:match('rebase'))
          and last_known_line > 1
          and last_known_line <= vim.api.nvim_buf_line_count(opts.buf)
        then
          vim.api.nvim_feedkeys([[g`"]], 'nx', false)
        end
      end,
    })
  end,
})

require("nvim-treesitter.configs").setup{
  highlight = { enable = true },
  indent = { enable = true },
  playground = { enable = true },
}

require('telescope').setup{
  pickers = {
    buffers = {
      sort_lastused = true
    }
  }
}

local tb = require('telescope.builtin')
vim.keymap.set('n', '<leader>sh', tb.help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sk', tb.keymaps, { desc = '[S]earch [K]eymaps' })
vim.keymap.set('n', '<leader>sf', tb.find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>ss', tb.builtin, { desc = '[S]earch [S]elect Telescope' })
vim.keymap.set('n', '<leader>sw', tb.grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', tb.live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', tb.diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sr', tb.resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader>s.', tb.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
vim.keymap.set('n', '<leader><leader>', tb.buffers, { desc = '[ ] Find existing buffers' })
