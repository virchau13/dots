{ inputs, configDir, config, pkgs, lib, ... }: 
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
    # Produces an expression that can be passed to `home.file` or
    # `xdg.configFile` that symlinks all dirs in `sourceDir` 
    # to the relative string `targetDir`.
    # Both arguments must be strings.
    # `sourceDir` is relative to `./.`.
    symlinkDirContents = sourceDir: targetDir: with pkgs.lib;
        mapAttrs' (name: _: 
            nameValuePair (targetDir + "/${name}") { source = mkOutOfStoreSymlink ("${configDir}/" + sourceDir + "/${name}"); }
        ) (builtins.readDir (./. + "/${sourceDir}"));
in {
    nixpkgs.overlays = [
        inputs.neovim-nightly-overlay.overlay
    ];

    xdg.configFile = {
    } // symlinkDirContents "apps/nvim" "nvim";

    home.file = {
        ".zshrc".source = ./apps/zsh/zshrc;
        ".zshenv".source = ./apps/zsh/zshenv;
        "bin".source = mkOutOfStoreSymlink "${configDir}/bin";
    };

    # Extra $PATH directories
    home.sessionPath = [ "${config.home.homeDirectory}/bin" ];

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
        plugins = with pkgs.vimPlugins; let 
            # COQ plugins are updated every day, autogenerate the version
            coq_nvim = (pkgs.vimUtils.buildVimPluginFrom2Nix {
                pname = "coq_nvim";
                version = with lib; builtins.head (
                        splitString " " (
                            last (
                                splitString
                                "\n"
                                (fileContents "${inputs.coq_nvim}/.github/.agp")
                                )
                            )
                        );
                src = inputs.coq_nvim;
                meta.homepage = inputs.coq_nvim.url;
            }).overrideAttrs (old: {
                buildInputs = [ pkgs.sqlite ];
                postInstall = ''
                    cd $out
                    sed -i 's/local is_xdg = .*/local is_xdg = true/' lua/coq.lua
                    CFGET="user_config = nvim.vars.get(SETTINGS_VAR, {})" 
                    substituteInPlace coq/server/runtime.py --replace \
                        "$CFGET" \
                        "$CFGET"'; user_config["xdg"] = True'
                    ${pkgs.python3}/bin/python3 -m coq deps
                '';
            });

            coq_artifacts = pkgs.vimUtils.buildVimPluginFrom2Nix {
                pname = "coq_artifacts";
                version = with lib; builtins.head (
                        splitString " " (
                            last (
                                splitString
                                "\n"
                                (fileContents "${inputs.coq_artifacts}/.github/.agp")
                                )
                            )
                        );
                src = inputs.coq_artifacts;
                meta.homepage = inputs.coq_artifacts.url;
            };

            coq_thirdparty = pkgs.vimUtils.buildVimPluginFrom2Nix {
                pname = "coq_thirdparty";
                version = with lib; builtins.head (
                        splitString " " (
                            last (
                                splitString
                                "\n"
                                (fileContents "${inputs.coq_thirdparty}/.github/.agp")
                                )
                            )
                        );
                src = inputs.coq_thirdparty;
                meta.homepage = inputs.coq_thirdparty.url;
            };
        in [
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
            # autocomplete
            coq_nvim
            coq_artifacts
            coq_thirdparty
        ];
        extraConfig = "lua require 'init'";
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
