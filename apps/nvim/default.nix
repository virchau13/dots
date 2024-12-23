{ inputs, configDir, config, pkgs, lib, ... }@args:
let
    utils = import ../../util.nix { inherit config pkgs configDir; };
    powershellLangBundle = pkgs.fetchzip {
        url = "https://github.com/PowerShell/PowerShellEditorServices/releases/download/v3.8.1/PowerShellEditorServices.zip";
        stripRoot = false;
        hash = "sha256-joHOXQ5sHfg6oR8IYYU9X2lIrxK20UrpsfAY5qwENqo=";
    };
in {

    xdg.configFile = {
    } // utils.symlinkDirContents "apps/nvim" "nvim";

    nixpkgs.overlays = [ inputs.neovim-nightly-overlay.overlays.default ];

    programs.neovim = {
        enable = true;
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
            extra.colorscheme
            # bar
            lualine-nvim
            # lsp configuration
            nvim-lspconfig
            # lsp pictograms
            lspkind-nvim
            # treesitter highlighting
            nvim-treesitter.withAllGrammars
            # text objects that allow you to play with treesitter nodes
            nvim-treesitter-textobjects
            # tree-sitter structural search replace
            ssr-nvim
            # easy way to play with tree-sitter queries
            extra.architext-nvim
            # fzf alternative
            telescope-nvim
            # peeks lines when you :<line number>
            numb-nvim
            # displays gitsigns on the left bar
            # (TODO check performance) gitsigns-nvim
            # autocomplete
            cmp-nvim-lsp
            cmp-buffer
            cmp-path
            cmp-cmdline
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
            # extra.sexy_scroller-vim
            # for proper indentation
            # i really hate the fact i have to install this >:(
            { 
                plugin = vim-polyglot;
                # vim-polyglot sets tabstop/shiftwidth to 2 unless set otherwise
                # and the nix plugin included also sets it to 2 so like don't thanks
                # ftdetect breaks filetype.lua so disable it
                config = ''
                    set ts=4 sw=4
                    let g:polyglot_disabled = ['nix', 'ftdetect']
                '';
            }
            # profiler
            extra.profile-nvim
            # LSP inlay hints
            extra.lsp-inlayhints-nvim

            # extra modes library
            hydra-nvim
            # box-drawing mode
            venn-nvim
            # auto insertion of closing braces { }
            nvim-autopairs
            # tab line, but for buffers
            bufferline-nvim
            # make jdtls language server extensions work with nvim
            nvim-jdtls
            # better netrw view
            oil-nvim
            # collection of plugins, currently used:
            # `mini.files`: better file manipulation on-the-fly
            mini-nvim
        ];
        extraConfig = ''
            lua require('init') { typescript = "${pkgs.nodePackages.typescript}", powershellEditorServices = "${powershellLangBundle}", --[[ dotnet = "{pkgs.dotnetCorePackages.sdk_7_0}", ]] openjdk8 = "${pkgs.openjdk8}" }
        '';
    };

}
