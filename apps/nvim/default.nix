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
            # easy way to view tree-sitter syntax trees
            playground
            # snippets
            luasnip
            # fzf alternative
            telescope-nvim
            # peeks lines when you :<line number>
            numb-nvim
            # displays gitsigns on the left bar
            (gitsigns-nvim.overrideAttrs(old: {
                # https://github.com/lewis6991/gitsigns.nvim/issues/604#issuecomment-1225896490
                src = pkgs.fetchFromGitHub {
                    owner = "lewis6991";
                    repo = "gitsigns.nvim";
                    rev = "1e107c91c0c5e3ae72c37df8ffdd50f87fb3ebfa";
                    sha256 = "sha256-d5kSdbqQBFdpu/Be+q6OqNNlGrgkL7OU13HOatLx4mE=";
                };
            }))
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
                config = "set ts=4 sw=4";
            }
        ];
        extraConfig = ''
            lua require('init') { typescript = "${pkgs.nodePackages.typescript}" }
        '';
    };

}
