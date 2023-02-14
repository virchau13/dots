# Custom packaging for plugins that aren't already in Nixpkgs.
{ inputs, pkgs, ... }: let
    simplePlugin = input: name: pkgs.vimUtils.buildVimPluginFrom2Nix {
        inherit name;
        src = input;
    };
in {
    parinfer-rust = let 
        raw-parinfer = (pkgs.callPackage (import "${inputs.parinfer-rust}/derivation.nix") {});
    in raw-parinfer.overrideAttrs(old: {
        postInstall = old.postInstall + ''
            ln -s $rtpPath/plugin $out/plugin
        '';
    });

    yuck-vim = simplePlugin inputs.yuck-vim "yuck.vim";

    fidget-nvim = simplePlugin inputs.fidget-nvim "fidget.nvim";

    virt-column-nvim = simplePlugin inputs.virt-column-nvim "virt-column.nvim";

    lsp_lines-nvim = simplePlugin inputs.lsp_lines-nvim "lsp_lines.nvim";

    sexy_scroller-vim = simplePlugin inputs.sexy_scroller-vim "sexy_scroller.vim";

    colorscheme = simplePlugin inputs.nvim-colorscheme "nvim_colorscheme";

    architext-nvim = simplePlugin inputs.architext-nvim "architext.nvim";

    profile-nvim = simplePlugin inputs.profile-nvim "profile.nvim";
}
