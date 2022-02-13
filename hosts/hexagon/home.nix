{ lib, config, homeDir, configDir, pkgs, ... }:
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in
{
    imports = [
        ../common/home.nix
        ../../apps/eww
    ];

    home.packages = let 
        xmonadAlias = pkgs.writeShellScriptBin "xmonad" ''
            #!${pkgs.bash}/bin/bash
            # So that `xmonad` knows how to restart itself.
            # The indirect `exec` rather than a symlink is required
            # because otherwise XMonad complains about not being called 'xmonad-x86_64-linux'.
            exec -a "$0" ~/.xmonad/xmonad-x86_64-linux "$@";
        '';
        # Work around https://github.com/Mic92/sops-nix/issues/150
        refresh-playlist = pkgs.writeShellScriptBin "refresh-playlist" ''
            exec /run/secrets/scripts/refresh-playlist "$@"
        '';
        packages = with pkgs; [
            xfce.thunar xfce.xfconf xfce.tumbler xfce.exo
            pavucontrol
            pass
            google-chrome
            discord-canary
            xbindkeys
            pamixer
            picom
            minecraft
            stack
            rofi
            notify-desktop
            mpc_cli
            obs-studio
            i3lock
            easyeffects
            peek
            lutris
            mpv
            wine
            winetricks
            transmission-qt
            polymc
            xmonadAlias
            refresh-playlist
            # Make discord run faster
            (pkgs.writeShellScriptBin "discord" ''
                ${pkgs.discord}/bin/discord \
                    --ignore-gpu-blocklist \
                    --disable-features=UseOzonePlatform \
                    --enable-features=VaapiVideoDecoder \
                    --use-gl=desktop \
                    --enable-gpu-rasterization \
                    --enable-zero-copy \
                    --no-sandbox
            '')
            # language servers
            sumneko-lua-language-server
        ];
        in packages;

    programs.alacritty = {
        enable = true;
        settings = {
            font.normal.family = "Hex Mono";
        };
    };

    systemd.user.sessionVariables = {
        DISPLAY = ":0";
    };

    services.mpd = {
        enable = true;
        musicDirectory = "${homeDir}/Music";
        extraConfig = ''
            audio_output {
                type "pulse"
                name "My Pulse Output"
            }
        '';
    };

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
    services.flameshot = {
        enable = true;
    };

    services.easyeffects = {
        enable = true;
    };

    # GTK themes
    gtk = {
        enable = true;
        # theme = {
        #     name = "Breeze";
        #     package = pkgs.gnome-breeze;
        # };
        iconTheme = {
            name = "Adwaita";
            package = pkgs.gnome.adwaita-icon-theme;
        };
    };

    # programs.firefox = {
    #     enable = true;
    #     profiles.dexkf0au = {
    #         id = 0;
    #         isDefault = true;
    #         userChrome = builtins.readFile ../../apps/firefox/userChrome.css;
    #     };
    # };

    home.file = {
        ".xinitrc".source = ../../apps/x11/xinitrc;
        ".xbindkeysrc".source = ../../apps/x11/xbindkeysrc;
        ".xprofile".source = ../../apps/x11/xprofile;
        ".xmonad".source = mkOutOfStoreSymlink "${configDir}/apps/xmonad";
        # More convenient link to ~/.config/nixpkgs.
        "config".source = mkOutOfStoreSymlink configDir;
    };
}
