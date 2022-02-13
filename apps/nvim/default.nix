{ inputs, configDir, config, pkgs, lib, ... }@args:
let
    utils = import ../../util.nix { inherit config pkgs configDir; };
in {

    xdg.configFile = {
    } // utils.symlinkDirContents "apps/nvim" "nvim";

    programs.neovim = {
        enable = true;
        package = pkgs.neovim-nightly;
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
            tokyonight-nvim
            # file browser (alternative to nerdtree)
            nvim-tree-lua
            # bar
            lightline-vim
            # lsp configuration
            nvim-lspconfig
            # lsp pictograms
            lspkind-nvim
            # treesitter highlighting
            nvim-treesitter
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
            # elixir syntax highlighting
            vim-elixir
            # lsp progress displayer
            extra.fidget-nvim
            # allows virtual text instead of a colorcolumn
            extra.virt-column-nvim
            # misc navigation bindings
            vim-unimpaired
        ];
        extraConfig = ''
            lua require('init') { typescript = "${pkgs.nodePackages.typescript}" }
        '';
    };

}
