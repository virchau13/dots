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
            xfce.thunar xfce.xfconf xfce.tumbler xfce.exo
            google-chrome
            xbindkeys
            picom
            stack
            rofi
            notify-desktop
            mpc_cli
            obs-studio
            i3lock
            lutris
            winetricks
            transmission_4-qt
            prismlauncher
            xcb-util-cursor
            refresh-playlist
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
            bspwm
            sxhkd
            # language servers
            sumneko-lua-language-server
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

    services.easyeffects = {
        enable = true;
    };

    home.pointerCursor = {
        gtk.enable = true;
        x11.enable = true;
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
        size = 16;
    };
    gtk.enable = true;
    dconf.settings = {
        "org/gnome/desktop/interface" = {
            cursor-theme = "Adwaita";
            font-antialiasing = "rgba";
        };
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
