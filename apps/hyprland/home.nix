{ inputs, config, configDir, pkgs, ... }:
let
    utils = import ../../util.nix { inherit config pkgs configDir; };
in
{
    xdg.configFile = {
        "hypr".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/apps/hyprland";
    };
}
