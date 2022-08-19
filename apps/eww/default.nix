{ config, pkgs, inputs, configDir, ... }: {
    home.packages = [ inputs.eww.packages."${pkgs.system}".default ];
    
    xdg.configFile = {
        "eww".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/apps/eww";
    };
}
