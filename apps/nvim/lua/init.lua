deps = {}
return function(deps_)
    deps = deps_
    require 'options'
    require 'plugin-settings'
    require 'tree-sitter-custom'
    require 'binds'
    require 'lsp'
    -- TODO remove
    vim.lsp.set_log_level('debug')

    vim.cmd("colorscheme tokyonight")
end
