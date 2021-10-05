require('formatter').setup {
    logging = false,
    filetype = {
        javascript = {
            function()
                return {
                    exe = "npx",
                    args = { "prettier", "--stdin-filepath", vim.api.nvim_buf_get_name(0), "--tab-width", "4" },
                    stdin = true
                }
            end
        },
    }
}
