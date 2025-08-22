local prettier = {
    function()
        return {
            exe = "npx",
            args = { "prettier", "--stdin-filepath", vim.api.nvim_buf_get_name(0), }, -- "--tab-width", "4" },
            stdin = true
        }
    end
}

require('formatter').setup {
    logging = false,
    filetype = {
        javascript = prettier,
        html = prettier,
        typescript = prettier,
        css = prettier,
        astro = prettier,
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
        c = {
            require('formatter.filetypes.c').clangformat
        },
        cpp = {
            require('formatter.filetypes.cpp').clangformat
        },
        python = {
            require('formatter.filetypes.python').black,
        },
        -- apply to all filetypes
        ["*"] = {
            require('formatter.filetypes.any').remove_trailing_whitespace
        },
    }
}


