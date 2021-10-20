{ lib, config, pkgs, ... }:
{
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "hexular";
    home.homeDirectory = "/home/hexular";


    imports = [
        ./common.nix
    ];

    nixpkgs.overlays = [
        (self: super: {
            # Enable GPU rasterization for Discord,
            # as well as no seccomp sandbox
            # (otherwise it lags too much.)
            discord-canary = super.discord-canary.overrideAttrs (old: {
                nativeBuildInputs = old.nativeBuildInputs ++ [
                    pkgs.nodePackages.asar
                ];
                buildPhase = 
                    let 
                      # Sanitize for sed in single quotes in bash.
                      replaceCode = builtins.replaceStrings 
                          ["'" "/" "&" "\n"] 
                          ["'\"'\"'" "\\/" "\\&" "\\n"] 
                          ''
                              for(let s of [
                                  "no-sandbox",
                                  "flag-switches-begin", 
                                  "enable-gpu-rasterization", 
                                  "flag-switches-end"
                              ]) {
                                  app.commandLine.appendSwitch(s);
                                  console.log("--" + s);
                              }
                          ''; 
                    in ''
                    ASAR_FILE="resources/app.asar"
                    asar extract "$ASAR_FILE" "$TMP/work"
                    sed -i "s/require('electron');"'/require("electron"); ${replaceCode}/' "$TMP/work/app_bootstrap/index.js"
                    asar pack "$TMP/work" "$ASAR_FILE"
                    '';
            });  
        })
    ];

    home.packages = let 
        packages = with pkgs; [
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

            # language servers
            sumneko-lua-language-server
        ];
        in packages;

    programs.alacritty = {
        enable = true;
    };

    systemd.user.sessionVariables = {
        DISPLAY = ":0";
    };

    services.mpd = {
        enable = true;
        musicDirectory = ~/Music;
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
        # More convenient link to ~/.config/nixpkgs.
        "config".source = config.lib.file.mkOutOfStoreSymlink ~/.config/nixpkgs;
    };

    # services.picom = {
    #     enable = true;
    #     backend = "glx";
    #     vSync = true;
    #     experimentalBackends = true;
    # };
}
