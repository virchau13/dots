deps = {}
hextmp={}
return function(deps_)
    -- TODO (eventually) remove this when all plugins update to be compatible
    vim.tbl_isarray = vim.isarray
    require 'profile-by-env'
    deps = deps_
    require 'options'
    require 'plugin-settings'
    require 'format'
    -- for astro development 
    -- require 'tree-sitter-custom'
    require 'binds'
    require 'lsp'
    require 'hydra-modes'

    -- require('one_monokai').setup {
    --     colors = {
    --         black = '#181b20'
    --     },
    --     highlights = function(colors)
    --         return {
    --             Normal = { bg = colors.black }
    --         }
    --     end
    -- }
    vim.cmd("colorscheme cyberdream")
end
