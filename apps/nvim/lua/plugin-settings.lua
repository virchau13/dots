require 'util'

-- Uses nerd-fonts extra symbols.
setgvar("lightline", {
    separator = {
        left = "\u{e0bc}",
        right = "\u{e0ba}"
    },
    subseparator = {
        left = "\u{e0bb}",
        right = "\u{e0bb}"
    },
    colorscheme = "tokyonight"
})
-- vim.fn["lightline#init"]()
-- vim.fn["lightline#colorscheme"]()
-- vim.fn["lightline#update"]()

require 'nvim-treesitter.configs'.setup {
    ensure_installed = "all",
    highlight = {
        enable = true,
        -- disable = { 'elixir' }
    },
    indent = {
        enable = true
    }
}

require('numb').setup()
require('gitsigns').setup()

local tree_cb = require'nvim-tree.config'.nvim_tree_callback
require'nvim-tree'.setup {
    -- closes neovim automatically when the tree is the last **WINDOW** in the view
    auto_close          = true,
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
        -- if true the tree will resize itself after opening a file
        auto_resize = true,
        mappings = {
            -- custom only false will merge the list with the default mappings
            -- if true, it will only use your list to set the mappings
            custom_only = false,
            -- list of mappings to set on the tree manually
            list = {
                { key = 't', cb = tree_cb("tabnew") }
            }
        },
        hide_root_folder = true
    }
}

require('fidget').setup {}

require('virt-column').setup()

setgvar("tokyonight_style", "night")
