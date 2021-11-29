{ inputs, config, pkgs, ... }:

{
    # Use a custom configuration.nix location.
    # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
    # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

    # Auto upgrade nix package and the daemon service.
    services.nix-daemon.enable = true;
    nix.package = pkgs.nixUnstable;

    # Create /etc/bashrc that loads the nix-darwin environment.
    programs.zsh.enable = true;  # default shell on catalina
    # programs.fish.enable = true;

    # Used for backwards compatibility, please read the changelog before changing.
    # $ darwin-rebuild changelog
    system.stateVersion = 4;

    users.users.virchaudhury = {
        home = "/Users/virchaudhury";
        name = "virchaudhury";
    };

    nix.trustedUsers = [ "virchaudhury" ];

    home-manager.extraSpecialArgs = let 
        homeDir = config.users.users.virchaudhury.home; 
    in { 
        inherit inputs homeDir;
        configDir = "${homeDir}/.config/nixpkgs";
    };
    home-manager.users.virchaudhury = { pkgs, ... }: {
        imports = [ ./common-home.nix ];
    };
}
