{ inputs, configDir, config, pkgs, lib, ... }: 
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in {
    imports = [
        ../../apps/nvim
    ];

    # An unfortunate, but necessary, line.
    nixpkgs.config.allowUnfree = true;

    nixpkgs.overlays = [
        inputs.neovim-nightly-overlay.overlay
    ];
    
    home.file = {
        ".zshrc".source = ../../apps/zsh/zshrc;
        ".zshenv".source = ../../apps/zsh/zshenv;
        "bin".source = mkOutOfStoreSymlink "${configDir}/bin";
    };

    # Extra $PATH directories
    home.sessionPath = [ "${config.home.homeDirectory}/bin" ];

    programs.git = {
        enable = true;
        userName = "virchau13";
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
