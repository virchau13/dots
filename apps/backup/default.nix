{ config, options, lib, pkgs, ... }:
let 
    timerConfigOptionDecl = 
        ((builtins.head (builtins.head options.services.restic.backups.type.getSubModules).imports) {name="remote";}).options.timerConfig;
in
{
    options.services.custom-backup = {
        enable = lib.mkEnableOption "Backblaze backup";
        timerConfig = lib.mkOption {
            description = ''systemd timer config'';
            type = timerConfigOptionDecl.type;
            default = timerConfigOptionDecl.default;
            example = timerConfigOptionDecl.example;
        };
    };
    config = lib.mkIf config.services.custom-backup.enable {
        services.restic = {
            backups = {
                remote = {
                    paths = [
                        "/var/lib"
                        "/home"
                        "/root"
                        # keys
                        "/etc/ssh"
                    ];
                    extraBackupArgs = [
                        "--cache-dir" "/var/cache/restic-cache"
                    ];
                    repositoryFile = "/run/secrets/backblaze/repo";
                    passwordFile = "/run/secrets/backblaze/pw";
                    environmentFile = "/run/secrets/backblaze/env";
                    initialize = true;
                    timerConfig = config.services.custom-backup.timerConfig;
                };
            };
        };

        sops.secrets = {
            "backblaze/env" = {};
            "backblaze/pw" = {};
            "backblaze/repo" = {};
        };
    };
}
