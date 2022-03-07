local lsp = require 'lspconfig'
local configs = require 'lspconfig.configs'
local util = require 'lspconfig.util'

function def_conf(name, opt)
    if not configs[name] then
        configs[name] = opt
    end
end

def_conf('glsl', {
    default_config = {
        cmd = { 'glslls', '--stdin' },
        filetypes = { 'vert', 'tesc', 'tese', 'geom', 'frag', 'comp'},
        root_dir = function(fname)
            return lsp.util.find_git_ancestor(fname) or vim.loop.os_homedir()
        end,
        settings = {}
    }
})

def_conf('astro', {
    default_config = {
        cmd = { 'npx', 'astro-ls', '--stdio' },
        filetypes = { 'astro' },
        root_dir = function(fname)
            return util.root_pattern('package.json', 'tsconfig.json', 'jsconfig.json', '.git')(fname)
        end,
        docs = {
            description = 'https://github.com/withastro/language-tools',
            root_dir = [[root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git")]],
        },
        settings = {},
    }
})
