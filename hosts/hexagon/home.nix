{ lib, config, homeDir, configDir, pkgs, ... }:
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in
{
    imports = [
        ../common/home.nix
        ../../apps/hyprland/home.nix
        ../../apps/eww
        ../../apps/fontconfig/home.nix
    ];

    home.packages = let 
        # Work around https://github.com/Mic92/sops-nix/issues/150
        refresh-playlist = pkgs.writeShellScriptBin "refresh-playlist" ''
            exec /run/secrets/scripts/refresh-playlist "$@"
        '';
        packages = with pkgs; [
            xfce.thunar xfce.xfconf xfce.tumbler xfce.exo
            pavucontrol
            pass
            google-chrome
            xbindkeys
            pamixer
            picom
            stack
            rofi
            notify-desktop
            mpc_cli
            obs-studio
            i3lock
            easyeffects
            lutris
            mpv
            winetricks
            transmission_4-qt
            wofi
            prismlauncher
            xcb-util-cursor
            refresh-playlist
            vesktop
            # Make discord run faster
            # (writeShellScriptBin "discord" ''
            #     ${discord}/bin/discord \
            #         --ignore-gpu-blocklist \
            #         --enable-gpu-rasterization \
            #         --enable-features=VaapiVideoDecoder \
            #         --use-gl=egl \
            #         --enable-zero-copy \
            #         --no-sandbox
            # '')
            vesktop
            bspwm
            sxhkd
            # language servers
            sumneko-lua-language-server
            pinentry.tty
            (pkgs.flameshot.overrideAttrs(old: {
                version = "12.2.0-alpha";
                src = pkgs.fetchFromGitHub {
                    owner = "flameshot-org";
                    repo = "flameshot";
                    rev = "3d21e4967b68e9ce80fb2238857aa1bf12c7b905";
                    sha256 = "sha256-OLRtF/yjHDN+sIbgilBZ6sBZ3FO6K533kFC1L2peugc=";
                };
                buildInputs = old.buildInputs ++ [ pkgs.libsForQt5.kguiaddons ];
                patches = [ ./flameshot-fix-clipboard.patch ];
                cmakeFlags = [ "-DUSE_WAYLAND_GRIM=1" "-DUSE_WAYLAND_CLIPBOARD=true" ];

                postInstall = ''
                    wrapProgram $out/bin/flameshot \
                        --prefix PATH : ${pkgs.grim}/bin
                '';
            }))
            java-language-server
            cargo-nextest
        ];
        in packages;

    home.sessionVariables = {
        # enable fcitx
        GTK_IM_MODULE = "fcitx";
        QT_IM_MODULE = "fcitx";
        XMODIFIERS = "@im=fcitx";
    };

    programs.alacritty = {
        enable = true;
        settings = {
            font.normal.family = "Hex Mono";
            font.size = 10.5;
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

    services.easyeffects = {
        enable = true;
    };

    home.pointerCursor = {
        gtk.enable = true;
        x11.enable = true;
        name = "Adwaita";
        package = pkgs.gnome.adwaita-icon-theme;
        size = 16;
    };
    gtk.enable = true;
    dconf.settings = {
        "org/gnome/desktop/interface" = {
            cursor-theme = "Adwaita";
            font-antialiasing = "rgba";
        };
    };

    services.gpg-agent = {
        enable = true;
        pinentryPackage = pkgs.pinentry.tty;
    };

    programs.tmux = {
        enable = true;
        clock24 = true;
        mouse = true;
        extraConfig = ''
            set-option -sa terminal-overrides ",xterm*:Tc"
        '';
    };

    # FIXME:
    # This really shouldn't be here...
    systemd.user.services.update-aquiladns = {
        Unit.Description = "Aquila DNS update Chechia IP";
        Service = {
            Type = "simple";
            ExecStart = pkgs.writeShellScript "ua" ''
                set -euo pipefail
                env
                IP=$(~/bin/ipv4)
                echo "sending $IP"
                ${pkgs.openssh}/bin/ssh -i ~/.ssh/id_rsa altair "echo '$IP' > /var/lib/dnscontrol/chechia-ip"
            '';
        };
    };
    systemd.user.timers.update-aquiladns = {
        Unit = {
            Description = "Aquila DNS update Chechia IP";
            After = [ "network.target" ];
        };
        Install.WantedBy = [ "multi-user.target" ];

        Timer = {
            OnUnitActiveSec = "10min";
            OnBootSec = "30";
        };
    };

    home.file = {
        ".xinitrc".source = ../../apps/x11/xinitrc;
        ".xbindkeysrc".source = ../../apps/x11/xbindkeysrc;
        ".xprofile".source = ../../apps/x11/xprofile;
        ".xmonad".source = mkOutOfStoreSymlink "${configDir}/apps/xmonad";
        # More convenient link to ~/.config/nixpkgs.
        "config".source = mkOutOfStoreSymlink configDir;
    };

    xdg.configFile = {
        "bspwm".source = mkOutOfStoreSymlink "${configDir}/apps/bspwm";
        # 'pluggable' mi amigo
        "sxhkd".source = mkOutOfStoreSymlink "${configDir}/apps/bspwm";
    };
}
