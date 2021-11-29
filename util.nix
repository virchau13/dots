{ config, pkgs, configDir }: 
{ 
    # Produces an expression that can be passed to `home.file` or
    # `xdg.configFile` that symlinks all dirs in `sourceDir` 
    # to the relative string `targetDir`.
    # Both arguments must be strings.
    # `sourceDir` is relative to `./.`.
    symlinkDirContents = sourceDir: targetDir: with pkgs.lib;
        mapAttrs' (name: _: 
            nameValuePair (targetDir + "/${name}") { source = config.lib.file.mkOutOfStoreSymlink ("${configDir}/" + sourceDir + "/${name}"); }
        ) (builtins.readDir (./. + "/${sourceDir}"));
}
