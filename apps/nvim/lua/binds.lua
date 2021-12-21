require 'util'

function noremap(mode, keys, bind, options)
    opts = { noremap = true }
    if options ~= nil then
        merge(opts, options)
    end

    vim.api.nvim_set_keymap(mode, keys, bind, opts)
end

noremap('n', '<Leader>nt', ':<C-u>NvimTreeToggle<CR>', { silent = true })
noremap('n', '<Leader>tn', ':<C-u>tabnew ')
noremap('n', '<Leader>tl', ':<C-u>Telescope<CR>')
-- Open files
noremap('n', '<Leader>op', ':<C-u>Telescope find_files<CR>')
-- Navigate buffers
noremap('n', '<Leader>b', ':<C-u>Telescope buffers<CR>')
-- Recursive grep
noremap('n', '<Leader>rg', ':<C-u>Telescope live_grep<CR>')
-- Quickfix list navigation
noremap('n', '<Leader>cn', ':<C-u>cnext<CR>')
noremap('n', '<Leader>cp', ':<C-u>cprev<CR>')
