require 'util'

require('lualine').setup {
    options = {
        theme = 'everblush'
    }
}

require 'nvim-treesitter.configs'.setup {
    -- ensure_installed = "all",
    -- auto_install = false,
    parser_install_dir = '~/.local/share/nvim/site',
    highlight = {
        enable = true,
        disable = function(lang, bufnr)
            -- disable in long buffers
            return vim.api.nvim_buf_line_count(bufnr) > 2000
        end,
        additional_vim_regex_highlighting = true -- to get vim-polyglot indentation to work
    },
    indent = {
        enable = false,
    },
    ignore_install = { 'go' },
}

require('numb').setup()
-- TODO check performance: require('gitsigns').setup()

local tree_cb = require'nvim-tree.config'.nvim_tree_callback
require'nvim-tree'.setup {
    -- opens the tree when changing/opening a new tab if the tree wasn't previously opened
    open_on_tab         = true,
    -- show lsp diagnostics in the signcolumn
    diagnostics = { enable = false },
    -- configuration options for the system open command (`s` in the tree by default)
    system_open = {
        -- the command to run this, leaving nil should work in most cases
        cmd  = nil,
        -- the command arguments as a list
        args = {}
    },

    view = {
        -- width of the window, can be either a number (columns) or a string in `%`
        width = 30,
        -- side of the tree, can be one of 'left' | 'right' | 'top' | 'bottom'
        side = 'left',
        mappings = {
            -- custom only false will merge the list with the default mappings
            -- if true, it will only use your list to set the mappings
            custom_only = false,
            -- list of mappings to set on the tree manually
            list = {
                -- (Bad practice apparently... I should be using buffers.)
                -- (But I just keep the keybind, just in case, you know?)
                { key = 't', cb = tree_cb("tabnew") }
            }
        },
        hide_root_folder = true
    },
}
-- automatically close nvim-tree when it's the last window
-- (https://github.com/kyazdani42/nvim-tree.lua/discussions/1115)
vim.api.nvim_create_autocmd("BufEnter", {
    nested = true,
    callback = function()
        if #vim.api.nvim_list_wins() == 1 and vim.api.nvim_buf_get_name(0):match("NvimTree_") ~= nil then
            vim.cmd "quit"
        end
    end
})

require('fidget').setup {}

require('virt-column').setup()

require('formatter').setup {
    filetype = {
        rust = {
            -- Rustfmt
            function()
                return {
                    exe = "rustfmt",
                    args = {"--emit=stdout", "--edition=2021"},
                    stdin = true
                }
            end
        },
        cpp = {
            require('formatter.filetypes.cpp').clangformat
        },
        -- apply to all filetypes
        ["*"] = {
            require('formatter.filetypes.any').remove_trailing_whitespace
        },
    }
}

-- require('lsp_lines').setup()
-- vim.diagnostic.config({
--     -- turn it off by default
--     virtual_lines = false,
-- })
