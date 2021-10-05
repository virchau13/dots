{ config, pkgs, ... }:

{
    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

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
            hexhls = pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; };
            packages = with pkgs; [
                neovim-nightly
                tmux
                neofetch
                pavucontrol
                google-chrome
                pass
                discord
                xbindkeys
                pamixer
                picom
                bat
                flameshot
                minecraft
                yarn
                nodejs
                peek
                dunst
                stack
                rofi
                notify-desktop
                mpc_cli

                # language servers
                sumneko-lua-language-server
                rnix-lsp
                rust-analyzer
                cmake-language-server
                ccls
                hexhls
            ];
            nodePackages = with pkgs.nodePackages; [
                firebase-tools
                ijavascript

                # language servers
                bash-language-server
                typescript-language-server
                dockerfile-language-server-nodejs
                vscode-json-languageserver
            ];
            haskellPackages = with pkgs.haskellPackages; [
            ];
        in packages ++ nodePackages ++ haskellPackages;

    programs.git = {
        enable = true;
        userName = "virchau13";
        userEmail = "virchau13@hexular.net";
    };

    programs.alacritty = {
        enable = true;
    };

    services.mpd = {
        enable = true;
        musicDirectory = ~/Music;
    };

    # Don't autostart.
    systemd.services.dunst.wantedBy = lib.mkForce [];
    services.dunst = {
        enable = true;
        settings = {
        };
    };

    # services.picom = {
    #     enable = true;
    #     backend = "glx";
    #     vSync = true;
    #     experimentalBackends = true;
    # };

    # services.flameshot.enable = true;

    xdg.configFile = {
        "nvim/init.vim".source = ./nvim/init.vim;
        "nvim/lua/lsp.lua".source = ./nvim/lua/lsp.lua;
        "nvim/lua/lsp-custom.lua".source = ./nvim/lua/lsp-custom.lua;
        "nvim/lua/format.lua".source = ./nvim/lua/format.lua;
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
