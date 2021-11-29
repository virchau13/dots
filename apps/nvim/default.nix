{ inputs, configDir, config, pkgs, lib, ... }:
let
    utils = import ../../util.nix { inherit config pkgs configDir; };
in {
    xdg.configFile = {
    } // utils.symlinkDirContents "apps/nvim" "nvim";

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

}
