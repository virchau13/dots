deps = {}
return function(deps_)
    require 'profile-by-env'
    deps = deps_
    require 'options'
    require 'plugin-settings'
    require 'format'
    require 'tree-sitter-custom'
    require 'binds'
    require 'lsp'
    require 'hydra-modes'

    require('one_monokai').setup {
        colors = {
            black = '#181b20'
        },
        themes = function(colors)
            return {
                Normal = { bg = colors.black }
            }
        end
    }
end
