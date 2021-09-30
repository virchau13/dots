{ config, pkgs, ... }:

{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    nixpkgs.overlays = [
        (import (builtins.fetchTarball {
            url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
        }))
    ];

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "hexular";
    home.homeDirectory = "/home/hexular";

    home.packages = let 
            packages = with pkgs; [
                neovim-nightly
                tmux
                cmake-language-server
                python-language-server
                # haskell-language-server (is buggy for some reason)
                sumneko-lua-language-server
                rnix-lsp
            ];
            nodePackages = with pkgs.nodePackages; [
                bash-language-server
                typescript-language-server
                dockerfile-language-server-nodejs
            ];
        in packages ++ nodePackages;

    programs.git = {
        enable = true;
        userName = "virchau13";
        userEmail = "virchau13@hexular.net";
    };

    xdg.configFile = {
        "nvim/init.vim".source = ./config/nvim/init.vim;
        "nvim/lua/lsp.lua".source = ./config/nvim/lua/lsp.lua;
        "nvim/lua/lsp-custom.lua".source = ./config/nvim/lua/lsp-custom.lua;
        "nvim/lua/format.lua".source = ./config/nvim/lua/format.lua;
    };

    home.file = {
        ".zshrc".source = ./zshrc;
        ".zshenv".source = ./zshenv;
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
