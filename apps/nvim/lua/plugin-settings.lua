require 'util'

require('lualine').setup {
    options = {
        -- theme = 'one_monokai'
        theme = 'auto' -- dynamic based on colorscheme
    }
}

require('bufferline').setup {}

if not os.getenv('NVIM_DISABLE_TS') then
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
            enable = { 'astro' },
        },
        incremental_selection = {
            enable = true,
            keymaps = {
                node_incremental = "<Tab>",
                node_decremental = "<S-Tab>",
            }
        }
    }
end

require('numb').setup()
-- TODO check performance: require('gitsigns').setup()

require('fidget').setup {}

require('virt-column').setup()

require('mini.files').setup()

-- require('lsp_lines').setup()
-- vim.diagnostic.config({
--     -- turn it off by default
--     virtual_lines = false,
-- })

-- require('lsp-inlayhints').setup()


require('typst-preview').setup()
