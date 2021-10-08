{ config, lib, pkgs, ... }:
{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    nixpkgs.overlays = [
        # neovim-nightly
        # (import (builtins.fetchTarball {
        #     url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
        # }))
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
                neofetch
                gnupg
                ripgrep
                pass
                bat

                # language servers
                rnix-lsp
                rust-analyzer
                # cmake-language-server
                ccls
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
            # Let GPG decide based on my email.
            key = "AA1BA03FFF02700DFD836BD325B242ED74B61B15";
        };
    };

    programs.neovim = {
        enable = true;
        # package = pkgs.neovim-nightly;
        withNodeJs = true;
        withPython3 = true;
        withRuby = true;
        plugins = with pkgs.vimPlugins; [
            nvim-web-devicons
            tokyonight-nvim
            nvim-tree-lua
            lightline-vim
            nvim-lspconfig
            # vim-firestore
            lspkind-nvim
            nvim-compe
            nvim-treesitter
            popup-nvim
            plenary-nvim
            telescope-nvim
            numb-nvim
            gitsigns-nvim
        ];
        extraConfig = "lua require 'init'";
    };

    xdg.configFile = {
        "nvim" = {
            source = ./apps/nvim;
            recursive = true;
        };
    };

    home.file = {
        ".zshrc".source = ./apps/zsh/zshrc;
        ".zshenv".source = ./apps/zsh/zshenv;
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
