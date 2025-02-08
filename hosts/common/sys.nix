{ inputs, config, lib, pkgs, ... }: {

    imports = [
        ../../apps/fontconfig
    ];

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    nix = {
        extraOptions = ''
            extra-experimental-features = nix-command flakes
            build-users-group = nixbld
        '';
        settings = {
          trusted-users = [ "root" "hexular" ];
          substituters = [
              "https://cache.nixos.org/"
              "https://nix-community.cachix.org"
          ];
          trusted-public-keys = [
              "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          ];
          sandbox = true;
        };
        # to get nix-index to use flakes
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    };
    
    environment.systemPackages = with pkgs; let 
            pythonPackages = pypkgs: with pypkgs; [
               ipython
               jupyter
               python-lsp-server
               z3
               fonttools
               black # formatter
               pylsp-mypy
               pylint
               imageio
               numpy
               pandas
               xkcdpass
               flask
               pyjwt
               pillow
               aiohttp
               scipy
               setuptools
               graphviz
               bcc
               transformers
               pytorch
               z3-solver
               gmpy2
               pygmt
               cython
               pycryptodome
               pwntools
               hypothesis
            ] ++ (if pkgs.system == "x86_64-linux" then [ i3ipc ] else []);
            packages = [
                zip
                (python3.withPackages pythonPackages)

                # lua
                lua

                # haskell
                haskell-language-server
                (ghc.withPackages (hs: with hs; [ QuickCheck ]))
                ihaskell

                # js
                nodejs
                yarn

                # elixir
                # elixir
                # elixir_ls

                # rust
                cargo
                rustc
                rustfmt
                clippy
                rust-analyzer

                # go
                go
                gopls

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
                unar
                imagemagick
                fd
                pass
                bat
                jq
                bind
                pv
                yt-dlp
                eza
                delta
                nmap
                tcptraceroute
                bintools
                figlet
                lolcat
                bash-completion
                nix-index
                tokei
                wezterm
                ffmpeg
                rlwrap
                lua-language-server

                ngn-k

                racket

                typst
                tinymist
                typst-live

                nil
                # sumneko-lua-language-server
                # cmake-language-server
                openjdk
                kotlin-language-server
                jdt-language-server

                firefox
                lshw
                wl-clipboard
                
                asm-lsp
                clang-tools

                ghostscript

                mosh

                zed-editor

                jujutsu
                nasm
            # sage
                cvc5
            ];
            nodePackages = with pkgs.nodePackages; [
                # firebase-tools
                pnpm
                # ijavascript

                # language servers
                bash-language-server
                typescript-language-server
                dockerfile-language-server-nodejs
                svelte-language-server
                vscode-langservers-extracted
            ];
       in packages ++ nodePackages;

    programs.zsh.enable = true;
}
