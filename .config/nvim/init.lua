vim.opt.mouse = 'a'

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- One statusline for the whole window, not one per split.
vim.o.laststatus = 3

local function statusline_diagnostics()
  local counts = vim.diagnostic.count(0)
  local out = {}
  for _, level in ipairs({ 'ERROR', 'WARN' }) do
    local n = counts[vim.diagnostic.severity[level]]
    if n then
      out[#out + 1] = level:sub(1, 1) .. n
    end
  end
  return #out > 0 and (' ' .. table.concat(out, ' ')) or ''
end

-- LSP is off until asked for, so the client list doubles as the indicator of
-- whether anything is running in this buffer.
local function statusline_lsp()
  local names = {}
  for _, client in ipairs(vim.lsp.get_clients({ bufnr = 0 })) do
    names[#names + 1] = client.name
  end
  if #names == 0 then
    return ''
  end
  table.sort(names)
  return ' [' .. table.concat(names, ' ') .. ']'
end

-- Global because 'statusline' reaches it through v:lua.
function _G.me_statusline()
  return table.concat({
    ' %<%f %m%r',
    '%=',
    statusline_diagnostics(),
    statusline_lsp(),
    ' %y %l:%c %P ',
  })
end

vim.o.statusline = '%!v:lua.me_statusline()'

vim.keymap.set('i', '<F3>', function()
  return vim.fn.strftime('%Y-%m-%d')
end, { expr = true, silent = true, desc = 'Insert date' })

vim.keymap.set('n', '<leader>e', function()
  local dir = vim.fn.expand('%:p:h') .. '/'
  vim.api.nvim_feedkeys(':edit ' .. dir, 'n', false)
end, { silent = true, desc = 'Edit current file dir' })

vim.keymap.set('n', '<leader>w', ':write<CR>')
vim.keymap.set('n', '<leader>l', ':ls<CR>:b ')

local diagnostic_goto = function(next, severity)
  return function()
    vim.diagnostic.jump({
      count = (next and 1 or -1) * vim.v.count1,
      severity = severity and vim.diagnostic.severity[severity] or nil,
      float = true,
    })
  end
end

vim.keymap.set("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
vim.keymap.set("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
vim.keymap.set("n", "[d", diagnostic_goto(false), { desc = "Prev Diagnostic" })
vim.keymap.set("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
vim.keymap.set("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev Error" })
vim.keymap.set("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
vim.keymap.set("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev Warning" })

local osc52 = require('vim.ui.clipboard.osc52')

vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight yanked text, and mirror real yanks to the terminal clipboard',
  group = vim.api.nvim_create_augroup('me-highlight-yank', { clear = true }),
  callback = function()
    vim.hl.on_yank()
    -- Only 'y': deletes and changes fill registers too, and shouldn't clobber
    -- the system clipboard.
    if vim.v.event.operator == 'y' then
      osc52.copy('+')(vim.v.event.regcontents)
    end
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

-- Parsers are installed by home-headless.nix, but installing one does not
-- attach it: highlighting needs an explicit start per buffer. pcall because it
-- throws for filetypes with no parser.
vim.api.nvim_create_autocmd('FileType', {
  group = vim.api.nvim_create_augroup('me-treesitter', { clear = true }),
  callback = function()
    pcall(vim.treesitter.start)
  end,
})

local Snacks = require('snacks')
Snacks.setup({
    bigfile = { enabled = true },
    explorer = { enabled = true },
    picker = {
        enabled = true,
        -- Route vim.ui.select through the picker instead of the numbered prompt.
        ui_select = true,
        win = {
            input = {
                keys = {
                    -- Close on the first Esc rather than dropping the input
                    -- field into normal mode. Snacks suggests exactly this in
                    -- its own defaults; its default binds Esc to cancel in
                    -- normal mode only, which is why the first press just left
                    -- insert. <C-c> stays bound to cancel.
                    ["<Esc>"] = { "close", mode = { "n", "i" } },
                },
            },
        },
    },
    notifier = { enabled = true }
})

local function apply_mappings(default_mode, specs)
  for _, spec in ipairs(specs) do
    local lhs = spec[1]
    local rhs = spec[2]
    local opts = {}
    for k, v in pairs(spec) do
      if type(k) ~= "number" then
        opts[k] = v
      end
    end
    local mode = opts.mode or default_mode
    opts.mode = nil

    if type(mode) == "table" then
      for _, m in ipairs(mode) do
        vim.keymap.set(m, lhs, rhs, opts)
      end
    else
      vim.keymap.set(mode, lhs, rhs, opts)
    end
  end
end

apply_mappings("n", {

    { "<leader>,", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>/", function() Snacks.picker.grep() end, desc = "Grep (Root Dir)" },
    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader><space>", function() Snacks.picker.files() end, desc = "Find Files (Root Dir)" },
    { "<leader>n", function() Snacks.picker.notifications() end, desc = "Notification History" },

    { "<leader>fb", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>fB", function() Snacks.picker.buffers({ hidden = true, nofile = true }) end, desc = "Buffers (all)" },
    { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files (Root Dir)" },
    { "<leader>fF", function() Snacks.picker.files({ root = false }) end, desc = "Find Files (cwd)" },
    { "<leader>fg", function() Snacks.picker.git_files() end, desc = "Find Files (git-files)" },
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
    { "<leader>fR", function() Snacks.picker.recent({ filter = { cwd = true }}) end, desc = "Recent (cwd)" },
    { "<leader>fp", function() Snacks.picker.projects() end, desc = "Projects" },

    { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
    { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Open Buffers" },
    { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep (Root Dir)" },
    { "<leader>sG", function() Snacks.picker.grep({ root = false }) end, desc = "Grep (cwd)" },
    { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Visual selection or word (Root Dir)", mode = { "n", "x" } },
    { "<leader>sW", function() Snacks.picker.grep_word({ root = false }) end, desc = "Visual selection or word (cwd)", mode = { "n", "x" } },

    { '<leader>s"', function() Snacks.picker.registers() end, desc = "Registers" },
    { '<leader>s/', function() Snacks.picker.search_history() end, desc = "Search History" },
    { "<leader>sa", function() Snacks.picker.autocmds() end, desc = "Autocmds" },
    { "<leader>sc", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>sC", function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
    { "<leader>sD", function() Snacks.picker.diagnostics_buffer() end, desc = "Buffer Diagnostics" },
    { "<leader>sh", function() Snacks.picker.help() end, desc = "Help Pages" },
    { "<leader>sH", function() Snacks.picker.highlights() end, desc = "Highlights" },
    { "<leader>si", function() Snacks.picker.icons() end, desc = "Icons" },
    { "<leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
    { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>sl", function() Snacks.picker.loclist() end, desc = "Location List" },
    { "<leader>sM", function() Snacks.picker.man() end, desc = "Man Pages" },
    { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
    { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume" },
    { "<leader>sq", function() Snacks.picker.qflist() end, desc = "Quickfix List" },
    { "<leader>su", function() Snacks.picker.undo() end, desc = "Undotree" },

    { "<leader>uC", function() Snacks.picker.colorschemes() end, desc = "Colorschemes" },

    { "<leader>fe", function() Snacks.explorer() end, desc = "Explorer (cwd)", },
    { "<leader>fE", function() Snacks.explorer.reveal() end, desc = "Explorer reveal", },

})

require('which-key').setup({
    preset = 'helix'
})
