{ inputs, config, lib, pkgs, ... }: {

    imports = [
        ../../apps/fontconfig
    ];

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    nix = {
        package = pkgs.nixUnstable;
        extraOptions = ''
            extra-experimental-features = nix-command flakes
            build-users-group = nixbld
        '';
        trustedUsers = [ "root" "hexular" ];
        binaryCaches = [
            "https://cache.nixos.org/"
            "https://nix-community.cachix.org"
        ];
        binaryCachePublicKeys = [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        ];
        useSandbox = true;
        # to get nix-index to use flakes
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    };
    
    environment.systemPackages = with pkgs; let 
            pythonPackages = python-pkgs: with python-pkgs; [
               ipython
               jupyter
               python-lsp-server
               z3
            ];
            python = python3.withPackages pythonPackages;
            packages = [
                python

                # lua
                lua

                # haskell
                stack

                # js
                nodejs
                yarn

                # elixir
                elixir
                elixir_ls

                # rust
                rustfmt

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
                imagemagick
                fd
                pass
                bat
                jq
                bind
                pv
                yt-dlp
                exa
                delta
                nmap
                tcptraceroute
                bintools
                ghc
                figlet
                lolcat
                tokei

                # language servers
                rnix-lsp
                rust-analyzer
                # sumneko-lua-language-server
                # cmake-language-server
                # (for compatibility with MacOS, because LLVM 13 is marked as broken there)
                (clang-tools.override { llvmPackages = pkgs.llvmPackages_12; })
                haskell-language-server
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

    programs.zsh.enable = true;
}
