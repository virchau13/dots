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
            pythonPackages = python-pkgs: with python-pkgs; [
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
               (buildPythonPackage rec {
                    pname = "types-Pillow";
                    version = "9.4.0.10";
                    src = fetchPypi {
                        inherit pname version;
                        sha256 = "sha256-NBwjRWELukUtFyR1fHuZemD1k88APBAbojnbADoK44k=";
                    };
                    meta = with lib; {
                        description = "Typing stubs for Pillow";
                        homepage = "https://github.com/python/typeshed";
                    };
               })
            ];
            python = python3.withPackages pythonPackages;
            packages = [
                python

                # lua
                lua

                # haskell
                haskell-language-server
                (haskellPackages.ghcWithPackages (pkgs: with pkgs; [ lens ]))

                # js
                nodejs
                yarn

                # elixir
                elixir
                elixir_ls

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
                exa
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

                rnix-lsp
                # sumneko-lua-language-server
                # cmake-language-server
            ];
            nodePackages = with pkgs.nodePackages; [
                # firebase-tools
                pnpm
                ijavascript

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
