require 'util'

-- Load in custom language server configs for those that don't exist in nvim-lspconfig.
require 'lsp-custom'

-- require('lspkind').init({
--     with_text = true,
--     symbol_map = {
--         Text = '',
--         Method = '',
--         Function = '',
--         Constructor = '',
--         Variable = '',
--         Class = '',
--         Interface = 'ﰮ',
--         Module = '',
--         Property = '',
--         Unit = '',
--         Value = '',
--         Enum = '',
--         Keyword = '',
--         Snippet = '﬌',
--         Color = '',
--         File = '',
--         Folder = '',
--         EnumMember = '',
--         Constant = '',
--         Struct = ''
--     }
-- })

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    local function buf_set_option(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = {noremap = true, silent = true}

    buf_set_keymap('n', '<space>ca', '<Cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<space>wa',
                   '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
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

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end



-- cmp.setup.cmdline(':', {
--     sources = cmp.config.sources({
--         { name = 'path' }
--     }, {
--         { name = 'cmdline' }
--     })
-- })

local settings = {
    clangd = {},
    tsserver = {},
    pylsp = {
        settings = {
            pylsp = {
                configurationSources = {"flake8"},
                plugins = {
                    pycodestyle = {
                        enabled = false -- Man i HATE python linters.
                    },
                    pylint = {enabled = false}
                }
            }
        }
    },
    rust_analyzer = {
        settings = {
            ["rust-analyzer"] = {
                cargo = {loadOutDirsFromCheck = true},
                procMacro = {enable = true}
            }
        }
    },
    omnisharp = {
        cmd = {'omnisharp', '--languageserver'}
    },
    cmake = {},
    texlab = {
        cmd = { 'texlab', '-vvvv', '--log-file', '/home/hexular/log' },
        settings = {
            texlab = {
                build = {
                    isContinuous = true,
                    args = { "-pvc", "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
                    executable = "latexmk",
                    onSave = true
                },
            }
        }
    },
    jdtls = {
        cmd = { 'bash', '-c', 'exec jdtls' }
    },
    bashls = {},
    html = {},
    haxe_language_server = {
        cmd = {'node', '~/prog/repos/haxe-language-server/bin/server.js'}
    },
    sumneko_lua = {
        cmd = {"lua-language-server"},
        settings = {
            Lua = {
                workspace = {
                    library = {
                        -- TODO fix
                        ['/usr/share/nvim/runtime/lua'] = true,
                        ['/usr/share/nvim/runtime/lua/lsp'] = true,
                    }
                },
                diagnostics = {
                    enable = true,
                    globals = {
                        "vim",
                    },
                    disable = "lowercase-global"
                },
                completion = {
                    enable = true,
                    workspaceWord = false
                }
            }
        }
    },
    glsl = {},
    -- (haskell-language-server)
    hls = {
        languageServerHaskell = {
            logFile = "/home/hexular/.hls-log"
        }
    },
    cssls = {},
    rnix = {},
    dockerls = {},
    jsonls = {},
    svelte = {},
}

vim.lsp.set_log_level('debug')
local capabilities = vim.lsp.protocol.make_client_capabilities()
local nvim_lsp = require('lspconfig')
for server, config in pairs(settings) do
    setup_obj = {
        on_attach = on_attach,
        capabilities = capabilities
    }
    merge(setup_obj, config)
    nvim_lsp[server].setup(setup_obj)
end

-- local cmp = require 'cmp'
-- local luasnip = require 'luasnip'
-- cmp.setup {
--     snippet = {
--         expand = function(args)
--             luasnip.lsp_expand(args.body)
--         end
--     },
--     mapping = {
--         ["<Tab>"] = cmp.mapping(function(fallback)
--             if cmp.visible() then
--                 cmp.select_next_item()
--             elseif luasnip.expand_or_jumpable() then
--                 luasnip.expand_or_jump()
--             elseif has_words_before() then
--                 cmp.complete()
--             else
--                 fallback()
--             end
--         end, { "i", "s" }),
--         ["<S-Tab>"] = cmp.mapping(function(fallback)
--             if cmp.visible() then
--                 cmp.select_prev_item()
--             elseif luasnip.jumpable(-1) then
--                 luasnip.jump(-1)
--             else
--                 fallback()
--             end
--         end, { "i", "s" }),
--         -- ['<CR>'] = cmp.mapping.confirm({ select = true }),
--     },
--     sources = cmp.config.sources({
--         { name = 'nvim_lsp' },
--         { name = 'luasnip' }
--     }, {
--         { name = 'buffer' }
--     }),
--     sorting = {
--         comparators = {
--             cmp.config.compare.offset,
--             cmp.config.compare.exact,
--             cmp.config.compare.score,
--             cmp.config.compare.recently_used,
--             -- prioritizes snippets, we don't want that
--             function (a, b)
--                 local res = cmp.config.compare.kind
--                 if kind == nil then
--                     return kind
--                 else
--                     return not kind
--                 end
--             end,
--             cmp.config.compare.sort_text,
--             cmp.config.compare.length,
--             cmp.config.compare.order,
--         }
--     },
--     formatting = {
--         format = require('lspkind').cmp_format({with_text = false, maxwidth = 50})
--     }
-- }
