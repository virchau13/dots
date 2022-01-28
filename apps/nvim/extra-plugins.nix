# Custom packaging for plugins that aren't already in Nixpkgs.
{ inputs, config, pkgs, lib, ... }: let
    simplePlugin = input: name: pkgs.vimUtils.buildVimPlugin {
        inherit name;
        src = input;
    };
in {
    # coq_nvim

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

    parinfer-rust = let 
        raw-parinfer = (pkgs.callPackage (import "${inputs.parinfer-rust}/derivation.nix") {});
    in raw-parinfer.overrideAttrs(old: {
        postInstall = old.postInstall + ''
            ln -s $rtpPath/plugin $out/plugin
        '';
    });

    yuck-vim = pkgs.vimUtils.buildVimPlugin {
        name = "yuck.vim";
        src = inputs.yuck-vim;
    };

    fidget-nvim = simplePlugin inputs.fidget-nvim "fidget.nvim";
}
