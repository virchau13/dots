require 'util'

require 'completion'
-- Load in custom language server configs for those that don't exist in nvim-lspconfig.
require 'lsp-custom'

local on_attach = require 'lsp-on-attach'

local has_words_before = function()
  local line, col = table.unpack(vim.api.nvim_win_get_cursor(0))
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

local function get_python_path(workspace)
    local path = require('lspconfig/util').path -- Use activated virtualenv.
    if vim.env.VIRTUAL_ENV then
        return path.join(vim.env.VIRTUAL_ENV, 'bin', 'python')
    end

    -- Find and use virtualenv in workspace directory.
    for _, pattern in ipairs({'*', '.*'}) do
        local match = vim.fn.glob(path.join(workspace, pattern, 'pyvenv.cfg'))
        if match ~= '' then
            return path.join(path.dirname(match), 'bin', 'python')
        end
    end

    -- Fallback to system Python.
    return vim.fn.exepath('python3') or vim.fn.exepath('python') or 'python'
end


local settings = {
    clangd = {
        cmd = clangd_cmd,
    },
    ts_ls = {
        cmd = { 'typescript-language-server', '--stdio', } -- '--tsserver-path', deps.typescript .. "/lib/node_modules/typescript/lib" }
    },
    pyright = {
        cmd = { 'npx', '--package=pyright', 'pyright-langserver', '--stdio' },
        before_init = function(_, config)
            config.settings.python.pythonPath = get_python_path(config.root_dir)
        end,
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
    -- omnisharp = {
    --     cmd = {'bash', '-c', 'DOTNET_ROOT=' .. deps.dotnet .. ' exec OmniSharp --languageserver'}
    -- },
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
    java_language_server = {
        cmd = {'/nix/store/klca0c3i4ad3jvd2ymv743nw2fs9zd4l-java-language-server-0.2.46/bin/java-language-server'}
    },
    racket_langserver = {
        cmd = {'bash', '-c', 'cd ~/prog/repos/racket-langserver; exec racket main.rkt'}
    },
    typst_lsp = {},
    -- this can take 6GB+ RAM, i don't have enough RAM for that
    -- kotlin_language_server = {},
}

local capabilities = require('cmp_nvim_lsp').default_capabilities()
local nvim_lsp = require('lspconfig')
for server, config in pairs(settings) do
    local setup_obj = {
        on_attach = on_attach,
        capabilities = capabilities
    }
    merge(setup_obj, config)
    nvim_lsp[server].setup(setup_obj)
end

vim.g.coq_settings = {
    -- auto_start = 'shut-up',
    limits = {
        -- 250 ms
        completion_auto_timeout = 0.250,
    },
    clients = {
        tree_sitter = { enabled = false, },
    },
    keymap = {
        -- this is set later, so it works with autopairs
        recommended = false
    }
}
