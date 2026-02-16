require 'util'

require('lualine').setup {
    options = {
        -- theme = 'one_monokai'
        theme = 'auto' -- dynamic based on colorscheme
    }
}

require('bufferline').setup {}

if not os.getenv('NVIM_DISABLE_TS') then
    -- require 'nvim-treesitter.configs'.setup {
    --     -- ensure_installed = "all",
    --     -- auto_install = false,
    --     parser_install_dir = '~/.local/share/nvim/site',
    --     highlight = {
    --         enable = true,
    --         disable = function(lang, bufnr)
    --             -- disable in long buffers
    --             return vim.api.nvim_buf_line_count(bufnr) > 2000
    --         end,
    --         additional_vim_regex_highlighting = true -- to get vim-polyglot indentation to work
    --     },
    --     indent = {
    --         -- enable = { 'astro' },
    --         enable = false,
    --     },
    --     incremental_selection = {
    --         enable = true,
    --         keymaps = {
    --             node_incremental = "<Tab>",
    --             node_decremental = "<S-Tab>",
    --         }
    --     }
    -- }
    
    local ts = require('nvim-treesitter')
    ts.setup()
    vim.api.nvim_create_autocmd('FileType', {
        pattern = ts.get_available(),
        callback = function(event)
            if vim.api.nvim_buf_line_count(event.buf) < 20000 then
                local filetype = vim.bo[event.buf].filetype
                local language = vim.treesitter.language.get_lang(filetype)
                if language and vim.treesitter.language.add(language) then
                    -- vim.treesitter.start()
                    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"

                    -- additional_vim_syntax_highlighting = true
                    -- vim.bo[event.buf].syntax = "ON"
                end
            end
        end
    });
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
