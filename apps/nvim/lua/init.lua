deps = {}
return function(deps_)
    deps = deps_
    require 'options'
    require 'plugin-settings'
    require 'format'
    require 'tree-sitter-custom'
    require 'binds'
    require 'lsp'

    vim.cmd("colorscheme everblush")
end
