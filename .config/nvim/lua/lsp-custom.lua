local lsp = require 'lspconfig'
local configs = require 'lspconfig/configs'

function def_conf(name, opt)
    if not lsp[name] then
        configs[name] = opt
    end
end

def_conf(
    'glsl',
    { default_config = {
        cmd = { 'glslls', '--stdin' },
        filetypes = { 'vert', 'tesc', 'tese', 'geom', 'frag', 'comp'},
        root_dir = function(fname)
            return lsp.util.find_git_ancestor(fname) or vim.loop.os_homedir()
        end,
        settings = {}
    } }
)
