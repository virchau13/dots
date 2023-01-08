local parser_config = require "nvim-treesitter.parsers".get_parser_configs()

-- For testing astro
parser_config.astro.install_info.url = "~/prog/repos/tree-sitter-astro"
