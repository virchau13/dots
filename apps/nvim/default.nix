{ inputs, configDir, config, pkgs, lib, ... }@args:
let
    utils = import ../../util.nix { inherit config pkgs configDir; };
in {

    xdg.configFile = {
    } // utils.symlinkDirContents "apps/nvim" "nvim";

    programs.neovim = {
        enable = true;
        package = inputs.neovim-nightly.packages."${pkgs.system}".default;
        plugins = with pkgs.vimPlugins; let 
            extra = import ./extra-plugins.nix args;
        in [
            # fs icons
            nvim-web-devicons
            # popups
            popup-nvim
            # async lua (required for telescope)
            plenary-nvim
            # colorscheme
            extra.everblush
            # file browser (alternative to nerdtree)
            nvim-tree-lua
            # bar
            lualine-nvim
            # lsp configuration
            nvim-lspconfig
            # lsp pictograms
            lspkind-nvim
            # treesitter highlighting
            nvim-treesitter 
            # easy way to view tree-sitter syntax trees
            playground
            # easy way to play with tree-sitter queries
            extra.architext-nvim
            # snippets
            luasnip
            # fzf alternative
            telescope-nvim
            # peeks lines when you :<line number>
            numb-nvim
            # displays gitsigns on the left bar
            gitsigns-nvim
            # autocomplete
            cmp-nvim-lsp
            cmp-buffer
            cmp-path
            nvim-cmp
            # snippets
            luasnip 
            # lisp expression indents
            extra.parinfer-rust
            # yuck syntax highlighting
            extra.yuck-vim
            # git operations
            vim-fugitive
            # lsp progress displayer
            extra.fidget-nvim
            # allows virtual text instead of a colorcolumn
            extra.virt-column-nvim
            # misc navigation bindings
            vim-unimpaired
            # easy formatting
            formatter-nvim
            # cool lines for diagnostics
            # extra.lsp_lines-nvim
            # smooth scrolling
            extra.sexy_scroller-vim
            # for proper indentation
            # i really hate the fact i have to install this >:(
            { 
                plugin = vim-polyglot;
                # vim-polyglot sets tabstop/shiftwidth to 2 unless set otherwise
                # and the nix plugin included also sets it to 2 so like don't thanks
                config = ''
                    set ts=4 sw=4
                    let g:polyglot_disabled = ['nix']
                '';
            }
        ];
        extraConfig = ''
            lua require('init') { typescript = "${pkgs.nodePackages.typescript}" }
        '';
    };

}
