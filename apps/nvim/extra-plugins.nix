# Custom packaging for plugins that aren't already in Nixpkgs.
{ inputs, config, pkgs, lib, ... }: let
    simplePlugin = input: name: pkgs.vimUtils.buildVimPlugin {
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
}
