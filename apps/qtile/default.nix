{ pkgs, config, configDir, ... }:
let
    utils = import ../../util.nix { inherit config pkgs configDir; };
in {
    xdg.configFile = {
    } // utils.symlinkDirContents "apps/qtile" "qtile";
}
