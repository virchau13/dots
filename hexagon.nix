{ lib, config, pkgs, ... }:
{
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "hexular";
    home.homeDirectory = "/home/hexular";

    imports = [
        ./common.nix
    ];

    home.packages = let 
        packages = with pkgs; [
            pavucontrol
            pass
            google-chrome
            discord
            xbindkeys
            flameshot
            pamixer
            picom
            minecraft
            stack
            rofi
            notify-desktop
            mpc_cli

            # language servers
            sumneko-lua-language-server
        ];
        in packages;

    programs.alacritty = {
        enable = true;
    };

    services.mpd = {
        enable = true;
        musicDirectory = ~/Music;
    };

    # Don't autostart.
    systemd.user.services.dunst.wantedBy = lib.mkForce {};
    services.dunst = {
        enable = true;
        settings = {
        };
    };

    services.flameshot.enable = true;

    home.file = {
        ".xinitrc".source = ./apps/x11/xinitrc;
        ".xbindkeysrc".source = ./apps/x11/xbindkeysrc;
        ".xprofile".source = ./apps/x11/xprofile;
        ".xmonad/build".source = ./apps/xmonad/build;
        # So that `xmonad` knows how to restart itself.
        # The indirect `exec` rather than a symlink is required
        # because otherwise XMonad complains about not being called 'xmonad-x86_64-linux'.
        "bin/xmonad" = {
            text = ''#!/usr/bin/env bash
            exec ~/.xmonad/xmonad-x86_64-linux "$@";
            '';
            executable = true;
        };
    };

    # services.picom = {
    #     enable = true;
    #     backend = "glx";
    #     vSync = true;
    #     experimentalBackends = true;
    # };

    # services.flameshot.enable = true;
}
