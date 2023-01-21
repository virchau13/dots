{ inputs, configDir, config, pkgs, lib, ... }: 
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in {
    imports = [
        ../../apps/nvim
    ];

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    home.file = {
        ".zshrc".source = mkOutOfStoreSymlink "${configDir}/apps/zsh/zshrc";
        ".zshenv".source = mkOutOfStoreSymlink "${configDir}/apps/zsh/zshenv";
        "bin".source = mkOutOfStoreSymlink "${configDir}/bin";
        ".cargo/config.toml".text = ''
            [build]
            rustc-wrapper = "${pkgs.sccache}/bin/sccache"
        '';
    };

    xdg.configFile = {
        "wezterm".source = mkOutOfStoreSymlink "${configDir}/apps/wezterm";
    };

    # Extra $PATH directories
    home.sessionPath = [ "${config.home.homeDirectory}/bin" ];

    programs.git = {
        enable = true;
        delta = {
            enable = true;
            options = {
                line-numbers = true;
                side-by-side = true;
                wrap-max-lines = "unlimited";
            };
        };
        userName = "Vir Chaudhury";
        userEmail = "virchau13@hexular.net";
        signing = {
            signByDefault = true;
            key = "AA1BA03FFF02700DFD836BD325B242ED74B61B15";
        };
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "21.05";
}
