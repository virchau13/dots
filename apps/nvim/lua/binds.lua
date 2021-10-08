require 'util'

vim.api.nvim_set_keymap('n', '<Leader>nt', ':<C-u>NvimTreeToggle<CR>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>tn', ':<C-u>tabnew', { noremap = true })
vim.api.nvim_set_keymap('n', '<Leader>tl', ':<C-u>Telescope<CR>', { noremap = true })
-- bind('n', '<Leader>op', ':<C-u>Telescope find_files<CR>')
