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

    nixpkgs.config.permittedInsecurePackages = [
        "python3.13-ecdsa-0.19.1" # Only used for playground/CTF solving
    ];
    
    environment.systemPackages = let 
            pythonPackageOverrides = self: super: {
            };
            pythonCustom = pkgs.python3.override { packageOverrides = pythonPackageOverrides; self = pythonCustom; };
            pythonPackages = pypkgs: with pypkgs; [
                ipython
                jupyter
                python-lsp-server
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
                z3-solver
                gmpy2
                pygmt
                cython
                pycryptodome
                pwntools
                hypothesis
                networkx
                opencv4
                cryptography
                pytest
                matplotlib
                scikit-learn
                scikit-image
#                kornia
#                transformers
#                thefuzz
#                easyocr
#                seaborn
#                dtw-python
#                ratelimit
#                backoff
#                ftfy
#                pdfplumber
#                flask-cors
#                shapely
#                onnx
#                paddlepaddle
#                paddleocr
#                paddlex
#                # paddlex[ocr]
#                einops imagesize jinja2 lxml openpyxl pyclipper pypdfium2 regex tiktoken tokenizers
                watchfiles
                pyqt6
                boto3
                ultralytics
                tifffile
                pooch
                rich
                qdrant-client
                pympler
                rq
                redis
                selenium
                stripe
                flask-socketio
                pymongo
                flask-mail
                galois
                elasticsearch
                flask-cors
                ecdsa
                tree-sitter
                tree-sitter-grammars.tree-sitter-cpp
            ] ++ (if pkgs.system == "x86_64-linux" then [ i3ipc ] else []);
            packages = with pkgs; [
                zip
                (pythonCustom.withPackages pythonPackages)

                # lua
                lua

                # haskell (TODO switch to latest GHC version)
                # https://github.com/haskell/haskell-language-server/issues/4674
                (haskell.packages.ghc9122.ghcWithPackages (hs: with hs; [ QuickCheck digits tardis haskell-language-server megaparsec ]))
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

                valgrind
                fzf
                swi-prolog
                frink
                ccache
                httpie
                chromedriver
                neovim-remote
                sage
                socat
                ghostty
                feh
                clang
                # jetbrains.idea-oss
                google-chrome
                llvmPackages.libllvm
                anki
                usbutils
                ncdu
                restic
                discord
                zed-editor
                ghidra-bin
            ];
            nodePackages = with pkgs.nodePackages; [
                # firebase-tools
                pnpm
                # ijavascript

                # language servers
                bash-language-server
                typescript-language-server
                # dockerfile-language-server
                svelte-language-server
                vscode-langservers-extracted
            ];
       in packages ++ nodePackages;

    programs.zsh.enable = true;
}
