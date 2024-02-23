{
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
            };
        };
    };

    sops.secrets = {
        "backblaze/env" = {};
        "backblaze/pw" = {};
        "backblaze/repo" = {};
    };
}
