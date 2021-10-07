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
            pamixer
            picom
            minecraft
            stack
            rofi
            notify-desktop
            mpc_cli
            i3lock
            easyeffects

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
            global = {
                monitor = 0;
                follow = "mouse";
                geometry = "300x5-30+20";
                progress_bar = true;
                progress_bar_height = 10;
                progress_bar_frame_width = 1;
                progress_bar_min_width = 150;
                progress_bar_max_width = 300;
                indicate_hidden = "yes";
                shrink = "no";
                transparency = 0;
                notification_height = 0;
                separator_height = 2;
                padding = 8;
                horizontal_padding = 8;
                text_icon_padding = 0;
                frame_width = 3;
                frame_color = "#aaaaaa";
                separator_color = "frame";
                sort = "yes";
                idle_threshold = 120;
                font = "Monospace 8";
                line_height = 0;
                markup = "full";
                format = "<b>%s</b>\\n%b";
                alignment = "left";
                vertical_alignment = "center";
                show_age_threshold = 60;
                word_wrap = "yes";
                ellipsize = "middle";
                ignore_newline = "no";
                stack_duplicates = true;
                hide_duplicate_count = false;
                show_indicators = "yes";

                icon_position = "left";
                min_icon_size = 0;
                max_icon_size = 32;
                sticky_history = "yes";
                history_length = 20;

            };
        };
    };

    systemd.user.services.flameshot.Unit.Requires = lib.mkForce [];
    services.flameshot.enable = true;

    services.polybar = {
        enable = true;
        script = ''
            export DISPLAY=:0
            polybar left &
            polybar right &
        '';
        settings = {
            "bar/left" = {
                monitor = "DVI-D-0";
                modules-right = "cpu memory";
            };
            "bar/right" = {
                monitor = "HDMI-0";
                modules-left = "xmonad-workspaces";
            };
            "module/cpu" = {
                type = "internal/cpu";
                interval = 5;
            };
            "module/memory" = {
                type = "internal/memory";
                interval = 5;
            };
            "module/xmonad-workspaces" = {
                type = "custom/script";
                exec = "tail -F /tmp/.xmonad-workspace-log";
                exec-if = "[ -p /tmp/.xmonad-workspace-log ]";
                tail = true;
            };
        };
    };

    services.easyeffects = {
        enable = true;
    };

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
}
