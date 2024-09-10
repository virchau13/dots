{ lib, config, homeDir, configDir, pkgs, ... }:
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in
{
    imports = [
        ../common/home.nix
        ../../apps/eww
    ];
}
