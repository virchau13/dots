{ config, pkgs, ... }:

{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    nixpkgs.overlays = [
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
                # editor
                neovim-nightly

                python

                # haskell
                stack

                # js
                nodejs
                yarn

                # misc
                tmux
                cmake
                neofetch
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
    };

    xdg.configFile = {
        "nvim/init.vim".source = ./apps/nvim/init.vim;
        "nvim/lua/lsp.lua".source = ./apps/nvim/lua/lsp.lua;
        "nvim/lua/lsp-custom.lua".source = ./apps/nvim/lua/lsp-custom.lua;
        "nvim/lua/format.lua".source = ./apps/nvim/lua/format.lua;
    };

    home.file = {
        ".zshrc".source = ./apps/zsh/zshrc;
        ".zshenv".source = ./apps/zsh/zshenv;
    };

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
