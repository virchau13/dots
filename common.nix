{ config, lib, pkgs, ... }:
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
    # Produces an expression that can be passed to `home.file` or
    # `xdg.configFile` that symlinks all dirs in `sourceDir` 
    # to the relative string `targetDir`.
    symlinkDirContents = sourceDir: targetDir: with pkgs.lib;
        mapAttrs' (name: _: 
            nameValuePair (targetDir + "/${name}") { source = mkOutOfStoreSymlink (sourceDir + "/${name}"); }
        ) (builtins.readDir sourceDir);
in
{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    nixpkgs.overlays = [
        # neovim-nightly
        (import (builtins.fetchTarball {
            url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
        }))
    ];

     home.packages = with pkgs; let 
            pythonPackages = python-pkgs: with python-pkgs; [
                ipython
                jupyter
            ];
            python = python3.withPackages pythonPackages;
            hexhls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; };
            packages = [
                python

                # lua
                lua

                # haskell
                stack

                # js
                nodejs
                yarn

                # misc
                tmux
                cmake
                file
                neofetch
                gnupg
                ripgrep
                unzip
                fd
                pass
                bat
                jq
                bind
                pv
                age

                # language servers
                rnix-lsp
                rust-analyzer
                sumneko-lua-language-server
                # cmake-language-server
                clang-tools
                hexhls
            ];
            nodePackages = with pkgs.nodePackages; [
                firebase-tools
                ijavascript

                # language servers
                bash-language-server
                typescript-language-server
                dockerfile-language-server-nodejs
                vscode-json-languageserver
            ];
        in packages ++ nodePackages;

    programs.git = {
        enable = true;
        userName = "virchau13";
        userEmail = "virchau13@hexular.net";
        signing = {
            signByDefault = true;
            key = "AA1BA03FFF02700DFD836BD325B242ED74B61B15";
        };
    };

    programs.neovim = {
        enable = true;
        package = pkgs.neovim-nightly;
        plugins = with pkgs.vimPlugins; [
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
            # autocomplete
            cmp-nvim-lsp
            cmp-buffer
            cmp-path
            nvim-cmp
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
        ];
        extraConfig = "lua require 'init'";
    };

    xdg.configFile = {
    } // symlinkDirContents ./apps/nvim "nvim";

    home.file = {
        ".zshrc".source = ./apps/zsh/zshrc;
        ".zshenv".source = ./apps/zsh/zshenv;
        "bin".source = mkOutOfStoreSymlink ./bin;
    };

    # Extra $PATH directories
    home.sessionPath = [ "${config.home.homeDirectory}/bin" ];

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "21.05";
}
