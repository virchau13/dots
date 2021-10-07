{ config, pkgs, ... }:

{
    imports = [
        ./common.nix
    ];

    home.packages = with pkgs; [
        gcc
    ];

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "virchaudhury";
    home.homeDirectory = "/Users/virchaudhury";

}
