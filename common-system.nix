{ inputs, config, lib, pkgs, ... }: {
    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;
    
     environment.systemPackages = with pkgs; let 
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
                coreutils
                gnupg
                ripgrep
                htop
                unzip
                fd
                pass
                bat
                jq
                bind
                pv

                # language servers
                rnix-lsp
                rust-analyzer
                # sumneko-lua-language-server
                # cmake-language-server
                (clang-tools.override { llvmPackages = pkgs.llvmPackages_12; })
                hexhls
            ];
            nodePackages = with pkgs.nodePackages; [
                # firebase-tools
                ijavascript

                # language servers
                bash-language-server
                typescript-language-server
                dockerfile-language-server-nodejs
                vscode-json-languageserver
                svelte-language-server
            ];
        in packages ++ nodePackages;
}
