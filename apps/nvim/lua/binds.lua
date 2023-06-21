require 'util'

function noremap(mode, keys, bind, options)
    opts = { noremap = true }
    if options ~= nil then
        merge(opts, options)
    end

    vim.api.nvim_set_keymap(mode, keys, bind, opts)
end

function telescope_bind(bind, builtin, opts)
    noremap('n', bind, ':<C-u>Telescope ' .. builtin .. '<CR>', opts)
end

noremap('n', '<Leader>nt', ':<C-u>NvimTreeToggle<CR>', { silent = true })
noremap('n', '<Leader>tn', ':<C-u>tabnew ')
noremap('n', '<Leader>tl', ':<C-u>Telescope<CR>')
-- noremap('n', '<Leader>f', ':<C-u>Format<CR>')
noremap('n', '<Leader>f', ':<C-u>FormatWrite<CR>')
noremap('n', '<Leader>w', '<C-w>') -- get around ttyd limitation

-- Coq autocomplete mappings
noremap('i', '<esc>', [[pumvisible() ? "<c-e><esc>" : "<esc>"]], { expr = true })
noremap('i', '<c-c>', [[pumvisible() ? "<c-e><c-c>" : "<c-c>"]], { expr = true })
noremap('i', '<tab>', [[pumvisible() ? "<c-n>" : "<tab>"]], { expr = true })
noremap('i', '<s-tab>', [[pumvisible() ? "<c-p>" : "<bs>"]], { expr = true })

-- Open files
telescope_bind('<Leader>op', 'find_files')
-- Navigate buffers
telescope_bind('<Leader>b', 'buffers')
-- Recursive grep
telescope_bind('<Leader>rg', 'live_grep')
-- LSP symbols
telescope_bind('<Leader>lws', 'lsp_workspace_symbols')
telescope_bind('<Leader>lds', 'lsp_document_symbols')
-- LSP diagnostics
telescope_bind('<Leader>ae', 'diagnostics')
