local prettier = {
    function()
        return {
            exe = "npx",
            args = { "prettier", "--stdin-filepath", vim.api.nvim_buf_get_name(0), "--tab-width", "4" },
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
        css = prettier
    }
}
