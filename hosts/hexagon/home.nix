{ lib, config, homeDir, configDir, pkgs, ... }:
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in
{
    imports = [
        ../common/home.nix
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
        xmonadAlias = pkgs.writeShellScriptBin "xmonad" ''
        #!${pkgs.bash}/bin/bash
        # So that `xmonad` knows how to restart itself.
        # The indirect `exec` rather than a symlink is required
        # because otherwise XMonad complains about not being called 'xmonad-x86_64-linux'.
        exec -a "$0" ~/.xmonad/xmonad-x86_64-linux "$@";
        '';
        devAlias = pkgs.writeShellScriptBin "dev" ''
        if [ -f ./flake.nix ]; then
            # flakes, use `nix develop`
            exec nix develop
        elif [ -f ./shell.nix -o -f ./default.nix ]; then
            # old nix, use `nix-shell`
            exec nix-shell
        else
            # do nothing
            echo "! No files detected, doing nothing"
        fi
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
            multimc
            xmonadAlias
            devAlias

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

    home.file = {
        ".xinitrc".source = ../../apps/x11/xinitrc;
        ".xbindkeysrc".source = ../../apps/x11/xbindkeysrc;
        ".xprofile".source = ../../apps/x11/xprofile;
        ".xmonad".source = mkOutOfStoreSymlink "${configDir}/apps/xmonad";
        # More convenient link to ~/.config/nixpkgs.
        "config".source = mkOutOfStoreSymlink configDir;
    };
}
