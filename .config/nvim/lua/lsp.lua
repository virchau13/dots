local nvim_lsp = require('lspconfig')
-- Load in custom language server configs for those that don't exist in nvim-lspconfig.
require'lsp-custom'

-- require('lspkind').init({
-- 	with_text = true,
-- 	symbol_map = {
-- 		Text = '',
-- 		Method = '',
-- 		Function = '',
-- 		Constructor = '',
-- 		Variable = '',
-- 		Class = '',
-- 		Interface = 'ﰮ',
-- 		Module = '',
-- 		Property = '',
-- 		Unit = '',
-- 		Value = '',
-- 		Enum = '了',
-- 		Keyword = '',
-- 		Snippet = '﬌',
-- 		Color = '',
-- 		File = '',
-- 		Folder = '',
-- 		EnumMember = '',
-- 		Constant = '',
-- 		Struct = ''
-- 	},
-- })

require'compe'.setup {
    enabled = true;
    autocomplete = true;
    debug = false;
    min_length = 1;
    preselect = 'enable';
    throttle_time = 80;
    source_timeout = 200;
    incomplete_delay = 400;
    max_abbr_width = 100;
    max_kind_width = 100;
    max_menu_width = 100;
    documentation = true;

    source = {
        path = true; buffer = true;
        calc = true;
        nvim_lsp = true;
        nvim_lua = true;
        vsnip = true;
    };
}

local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-n>"
    elseif vim.fn.call("vsnip#available", {1}) == 1 then
        return t "<Plug>(vsnip-expand-or-jump)"
    elseif check_back_space() then
        return t "<Tab>"
    else
        return vim.fn['compe#complete']()
    end
end
_G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-p>"
    elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
        return t "<Plug>(vsnip-jump-prev)"
    else
        return t "<S-Tab>"
    end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})


local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = { noremap=true, silent=true }

    buf_set_keymap('n', '<leader>ca', '<Cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
    buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)

    -- Set some keybinds conditional on server capabilities
    if client.resolved_capabilities.document_formatting then
        buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    elseif client.resolved_capabilities.document_range_formatting then
        buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
    end

end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local servers = {'ccls', 'tsserver', 'pyls', 'rust_analyzer', 'omnisharp', 'cmake', 'texlab', 'jdtls', 'bashls', 'html', 'haxe_language_server', 'sumneko_lua', 'glsl'}
local settings = {
    -- ccls
    {},
    -- tsserver
    {},
    -- pyls
    { settings = {
            pyls = {
                configurationSources = { "flake8" },
                plugins = {
                    pycodestyle = {
                        enabled = false, -- Man i HATE python linters.
                    },
                    pylint = {
                        enabled = false,
                    },
                }
            }
    } },
    -- rust_analyzer
    {
        settings = {
            ["rust-analyzer"] = {
                cargo = {
                    loadOutDirsFromCheck = true,
                },
                procMacro = {
                    enable = true,
                }
            }
        }
    },
    -- omnisharp
    {
        cmd = { 'omnisharp', '--languageserver' }
    },
    -- cmake
    {},
    -- texlab
    {},
    -- jdtls
    {},
    -- bashls
    {},
    -- html
    {
        cmd = {'vscode-html-languageserver', '--stdio'},
    },
    -- haxe_language_server
    {
        cmd = {'node', '~/prog/github/haxe-language-server/bin/server.js'}
    },
    -- sumneko_lua
    {
        cmd = {"lua-language-server"},
        settings = {
            Lua = {
                workspace = {
                    library = {
                        ['/usr/share/nvim/runtime/lua'] = true,
                        ['/usr/share/nvim/runtime/lua/lsp'] = true,
                        ['/usr/share/awesome/lib'] = true
                    }
                };
                diagnostics = {
                    enable = true;
                    globals = {
                        -- VIM
                        "vim",
                        "use", -- Packer use keyword
                        -- AwesomeWM
                        "awesome",
                        "client",
                        "root"
                    };
                    disable = "lowercase-global"
                };
            }
    }},
    -- glsl
    {}
}

-- Merges ...dictionaries? objects? tables?
function merge(a, b)
    for k, v in pairs(b) do
        a[k] = v
    end
end

for i, lsp in ipairs(servers) do
    setup_obj = {
        on_attach = on_attach,
        capabilities = capabilities,
    }
    merge(setup_obj, settings[i])
    nvim_lsp[lsp].setup(setup_obj)
end
