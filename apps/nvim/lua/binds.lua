function bind(mode, keys, cmd, silent)
    vim.api.nvim_set_keymap(mode, keys, cmd, { noremap = true, silent = silent })
end
