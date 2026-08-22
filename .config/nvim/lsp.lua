-- Concatenated onto init.lua by home-headless.nix, so this continues the same
-- Lua chunk: pkgs_*, Snacks and apply_mappings are still in scope here.

-- Servers are registered here but never enabled at startup: LSP is off until
-- <leader>ls, :LspStart or :LspStop says otherwise.
local lsp_servers = {
  ty = {
    cmd = { pkgs_ty, 'server' },
    filetypes = { 'python' },
    root_markers = { 'ty.toml', 'pyproject.toml', 'setup.py', 'setup.cfg', 'requirements.txt', '.git', '.venv' },
  },
  metals = {
    cmd = { pkgs_metals },
    filetypes = { 'scala', 'sbt' },
    root_markers = { 'build.sbt', 'build.sc', 'build.mill', '.scala-build', '.git' },
    init_options = {
      statusBarProvider = 'off',
      isHttpEnabled = true,
      compilerOptions = { snippetAutoIndent = false },
    },
  },
  ts_ls = {
    cmd = { pkgs_typescript_language_server, '--stdio' },
    filetypes = {
      'javascript', 'javascriptreact', 'javascript.jsx',
      'typescript', 'typescriptreact', 'typescript.tsx',
    },
    root_markers = { 'tsconfig.json', 'jsconfig.json', 'package.json', '.git' },
    init_options = { hostInfo = 'neovim' },
  },
}

for name, config in pairs(lsp_servers) do
  vim.lsp.config(name, config)
end

-- Enabling retro-attaches to open buffers, disabling stops the running clients.
local function lsp_complete(arg_lead)
  return vim.tbl_filter(function(name)
    return name:find(arg_lead, 1, true) == 1
  end, vim.tbl_keys(lsp_servers))
end

-- No name starts whatever is configured for the current filetype, never
-- everything: an unrelated server has no business attaching to this buffer.
local function lsp_start(names)
  if #names == 0 then
    local ft = vim.bo.filetype
    for name, server in pairs(lsp_servers) do
      if vim.tbl_contains(server.filetypes or {}, ft) then
        names[#names + 1] = name
      end
    end
    if #names == 0 then
      vim.notify(('No LSP configured for filetype %q'):format(ft), vim.log.levels.WARN)
      return
    end
  end
  table.sort(names)
  vim.lsp.enable(names)
  vim.notify('LSP on: ' .. table.concat(names, ', '))
end

-- No name stops every attached client, including any this config never
-- configured, so it always works as the get-out-of-my-way key.
local function lsp_stop(names)
  if #names == 0 then
    for _, client in ipairs(vim.lsp.get_clients()) do
      if not vim.tbl_contains(names, client.name) then
        names[#names + 1] = client.name
      end
    end
    if #names == 0 then
      vim.notify('No LSP clients attached', vim.log.levels.WARN)
      return
    end
  end
  table.sort(names)
  vim.lsp.enable(names, false)
  vim.notify('LSP off: ' .. table.concat(names, ', '))
end

vim.api.nvim_create_user_command('LspStart', function(opts)
  lsp_start(opts.fargs)
end, { nargs = '*', complete = lsp_complete, desc = 'Start LSP servers' })

vim.api.nvim_create_user_command('LspStop', function(opts)
  lsp_stop(opts.fargs)
end, { nargs = '*', complete = lsp_complete, desc = 'Stop LSP servers' })

-- Nvim already maps grn/gra/grr/gri/grt/K out of the box, so only the gaps are
-- filled here. Nothing binds bare `gr`: that would shadow the `gr*` defaults.
local lsp_keys = {
  { 'gd', function() Snacks.picker.lsp_definitions() end, desc = 'Goto Definition' },
  { 'gD', function() Snacks.picker.lsp_declarations() end, desc = 'Goto Declaration' },
  { 'gy', function() Snacks.picker.lsp_type_definitions() end, desc = 'Goto Type Definition' },
  -- Capital is the narrower scope here, matching sd/sD and ff/fF.
  { '<leader>ss', function() Snacks.picker.lsp_workspace_symbols() end, desc = 'Symbols (Workspace)' },
  { '<leader>sS', function() Snacks.picker.lsp_symbols() end, desc = 'Symbols (Buffer)' },

  -- K and insert-mode <C-s> are already nvim defaults, as are grn and gra.
  { 'gK', vim.lsp.buf.signature_help, desc = 'Signature Help' },
  { '<leader>ca', vim.lsp.buf.code_action, mode = { 'n', 'x' }, desc = 'Code Action' },
  { '<leader>cr', vim.lsp.buf.rename, desc = 'Rename' },
  { '<leader>ci', function() Snacks.picker.lsp_incoming_calls() end, desc = 'Incoming Calls' },
  { '<leader>co', function() Snacks.picker.lsp_outgoing_calls() end, desc = 'Outgoing Calls' },
  { '<leader>cc', vim.lsp.codelens.run, desc = 'Run Code Lens' },
  { '<leader>cC', vim.lsp.codelens.refresh, desc = 'Refresh Code Lens' },
  { '<leader>uh', function()
      local buf = vim.api.nvim_get_current_buf()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = buf }), { bufnr = buf })
    end, desc = 'Toggle Inlay Hints' },
}

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('me-lsp-keys', { clear = true }),
  callback = function(ev)
    apply_mappings('n', vim.tbl_map(function(spec)
      return vim.tbl_extend('force', spec, { buffer = ev.buf })
    end, lsp_keys))
  end,
})

-- Stopping the LSP must give `gd` and friends back to plain vim, so drop the
-- buffer-local maps once the last client leaves.
vim.api.nvim_create_autocmd('LspDetach', {
  callback = function(ev)
    vim.schedule(function()
      if
        vim.api.nvim_buf_is_valid(ev.buf)
        and #vim.lsp.get_clients({ bufnr = ev.buf }) == 0
      then
        for _, spec in ipairs(lsp_keys) do
          local mode = spec.mode or 'n'
          for _, m in ipairs(type(mode) == 'table' and mode or { mode }) do
            pcall(vim.keymap.del, m, spec[1], { buffer = ev.buf })
          end
        end
      end
    end)
  end,
})

-- Global, not buffer-local like lsp_keys: these have to work before any client
-- has attached.
-- <leader>l is the server itself: start, stop, inspect. <leader>c stays for
-- things that act on the code under the cursor.
vim.keymap.set('n', '<leader>ls', function() lsp_start({}) end, { desc = 'Start LSP (filetype)' })
vim.keymap.set('n', '<leader>lS', function() lsp_stop({}) end, { desc = 'Stop LSP (all attached)' })
vim.keymap.set('n', '<leader>li', '<cmd>checkhealth vim.lsp<cr>', { desc = 'LSP Status' })
vim.keymap.set('n', '<leader>ll', function() Snacks.picker.lsp_config() end, { desc = 'LSP Config' })
