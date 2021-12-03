{ config, pkgs, inputs, configDir, ... }: {
    home.packages = [ inputs.eww.defaultPackage."${pkgs.system}" ];
    
    xdg.configFile = {
        "eww".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/apps/eww";
    };
}
