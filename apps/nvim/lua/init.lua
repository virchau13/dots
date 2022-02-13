deps = {}
return function(deps_)
    deps = deps_
    require 'options'
    require 'plugin-settings'
    require 'binds'
    require 'lsp'

    vim.cmd("colorscheme tokyonight")
end
