{ config, pkgs, ... }:

{
    imports = [
        ./common.nix
    ];

    
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "hexular";
    home.homeDirectory = "/home/hexular";

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
}
