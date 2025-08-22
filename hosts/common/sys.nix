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
    
    environment.systemPackages = let 
            pythonPackageOverrides = self: super: {
                torch = super.torch.override { 
                    # triton = super.triton-no-cuda;
                    rocmSupport = true;
                    cudaSupport = false; 
                };
                #torch = super.torch-bin.overridePythonAttrs(old: {
                #    src = pkgs.fetchurl {
                #        name = "torch-2.7.1+rocm6.3-cp312-cp312-manylinux_2_28_x86_64.whl";
                #        url = "https://download.pytorch.org/whl/rocm6.3/torch-2.7.1%2Brocm6.3-cp312-cp312-manylinux_2_28_x86_64.whl#sha256=b0c10342f64a34998ae8d5084aa1beae7e11defa46a4e05fe9aa6f09ffb0db37";
                #        hash = "";
                #    }; 
                #    passthru = old.passthru // {
                #        cudaSupport = false;
                #        rocmSupport = true;
                #    };
                #});
                #torchvision = super.torchvision-bin.overridePythonAttrs(old: {
                #    buildInputs = old.buildInputs ++ (with pkgs; [
                #        rocmPackages.clr
                #        hip
                #    ]);
                #    src = pkgs.fetchurl {
                #        name = "torchvision-0.22.1+rocm6.3-cp312-cp312-manylinux_2_28_x86_64.whl";
                #        url = "https://download.pytorch.org/whl/rocm6.3/torchvision-0.22.1%2Brocm6.3-cp312-cp312-manylinux_2_28_x86_64.whl#sha256=0dce205fb04d9eb2f6feb74faf17cba9180aff70a8c8ac084912ce41b2dc0ab7";
                #        hash = "sha256-Dc4gX7BNnrL2/rdPrxfLqRgK/3CoyKwISRLOQbLcCrc=";
                #    };
                #});
                pymupdf = (pkgs.python3.pkgs.callPackage ../../apps/pymupdf-fix/package.nix {
                    mupdf = inputs.nixpkgs-pymupdf.legacyPackages.x86_64-linux.mupdf;
                });
                mahotas = super.mahotas.overridePythonAttrs { doCheck = false; };
            };
            pythonCustom = pkgs.python3.override { packageOverrides = pythonPackageOverrides; self = pythonCustom; };
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
                torch
                torchvision
                kornia
                transformers
                thefuzz
                easyocr
                seaborn
                dtw-python
                ratelimit
                backoff
                ftfy
                pdfplumber
                flask-cors
                shapely
                onnx
                paddlepaddle
                paddleocr
                paddlex
                # paddlex[ocr]
                einops imagesize jinja2 lxml openpyxl pyclipper pypdfium2 regex tiktoken tokenizers
                watchfiles
                pyqt6
                boto3
                ultralytics
                tifffile
                pooch
                rich
                qdrant-client
                pympler
                mlflow
                onnxruntime
                mahotas
                guppy3
                rq
                redis
                selenium
                stripe
                flask-socketio
                pymongo
                flask-mail
                galois
                elasticsearch
            ] ++ (if pkgs.system == "x86_64-linux" then [ i3ipc ] else []);
            packages = with pkgs; [
                zip
                (pythonCustom.withPackages pythonPackages)

                # lua
                lua

                # haskell
                haskell-language-server
                (ghc.withPackages (hs: with hs; [ QuickCheck sdl2 digits tardis ]))
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
                qdrant
                httpie
                chromedriver
                neovim-remote
                sage
                socat
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
