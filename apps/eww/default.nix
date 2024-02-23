{ config, pkgs, inputs, configDir, ... }: {
    home.packages = with pkgs; [ eww-wayland ];
    
    xdg.configFile = {
        "eww".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/apps/eww";
    };
}
