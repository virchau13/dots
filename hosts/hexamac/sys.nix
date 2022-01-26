{ inputs, config, pkgs, ... }:

{
    # Use a custom configuration.nix location.
    # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
    # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

    # Auto upgrade nix package and the daemon service.
    services.nix-daemon.enable = true;

    # Create /etc/bashrc that loads the nix-darwin environment.
    programs.zsh.enable = true;  # default shell on catalina
    # programs.fish.enable = true;

    # Used for backwards compatibility, please read the changelog before changing.
    # $ darwin-rebuild changelog
    system.stateVersion = 4;

    users.users.hexular = {
        home = "/Users/hexular";
        name = "hexular";
    };

    nix.trustedUsers = [ "hexular" ];

    environment.variables = {
        TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
    };

    home-manager.extraSpecialArgs = let 
        homeDir = config.users.users.hexular.home; 
    in { 
        inherit inputs homeDir;
        configDir = "${homeDir}/config";
    };
    home-manager.users.hexular = { pkgs, ... }: {
        imports = [ ../common/home.nix ];
    };
}
