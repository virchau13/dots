require 'util'

-- Load in custom language server configs for those that don't exist in nvim-lspconfig.
require 'lsp-custom'

local on_attach = function(client, bufnr)
    -- turn off lsp highlighting
    client.server_capabilities.semanticTokensProvider = nil

    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    local function buf_set_option(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = {noremap = true, silent = true}

    buf_set_keymap('n', '<Leader>ca', '<Cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('v', '<Leader>ca', '<Cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<Leader>wa',
                   '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<Leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<Leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
    buf_set_keymap('n', '<Leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<Leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<Leader>e', '<cmd>lua vim.diagnostic.open_float(0, { scope = "line", border = "single" })<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev({ float =  { border = "single" }})<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next({ float =  { border = "single" }})<CR>', opts)
    buf_set_keymap('n', '<Leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
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

local clangd_cmd = { 'clangd' }
-- detect custom $CC/$CXX and add that, so clangd doesn't error in a devshell
if vim.env.CC ~= nil or vim.env.CXX ~= nil then
    local path = vim.fn.exepath(vim.env.CC or vim.env.CXX)
    clangd_cmd = { 'clangd', '--query-driver', path }
end

local settings = {
    clangd = {
        cmd = clangd_cmd,
    },
    tsserver = {
        cmd = { 'typescript-language-server', '--stdio', '--tsserver-path', deps.typescript .. "/lib/node_modules/typescript/lib" }
    },
    pylsp = {
        cmd = { 'pylsp', '-vvv', '--log-file', '/home/hexular/.local/state/pylsp.log' },
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
                cargo = {loadOutDirsFromCheck = true, features = 'full'},
                procMacro = {enable = true},
                expressionAdjustmentHints = { enable = true },
                lifetimeElisionHints = { enable = true },
            }
        }
    },
    omnisharp = {
        cmd = {'bash', '-c', 'DOTNET_ROOT=' .. deps.dotnet .. ' exec OmniSharp --languageserver'}
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
    java_language_server = {
        cmd = { 'java-language-server' }
    },
    bashls = {},
    html = {},
    haxe_language_server = {
        cmd = {'node', '~/prog/repos/haxe-language-server/bin/server.js'}
    },
    lua_ls = {
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
        cmd = { 'haskell-language-server-wrapper', '--lsp', '-d', '-l', '/home/hexular/hlslog' },
        languageServerHaskell = {
            logFile = "/home/hexular/.hls-log"
        }
    },
    cssls = {},
    dockerls = {},
    jsonls = {},
    svelte = {},
    astro = {
        -- cmd = { 'npx', 'astro-ls', '--stdio' },
        cmd = {'bash', '-c', 'node /home/hexular/prog/repos/language-tools/packages/language-server/dist/node.js --stdio'}
    },
    gopls = {},
    wgsl_analyzer = {
        cmd = {'/home/hexular/prog/repos/wgsl-analyzer/target/release/wgsl_analyzer'}
    },
    powershell_es = {
        bundle_path = deps.powershellEditorServices
    },
    nginx = {},
    terraformls = {},
    nil_ls = {},
    kotlin_language_server = {},
}

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

vim.g.coq_settings = {
    auto_start = 'shut-up',
    xdg = true,
    clients = {
        tree_sitter = { enabled = false, },
    },
}
