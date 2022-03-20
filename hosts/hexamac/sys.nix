{ inputs, config, pkgs, lib, ... }:

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

    networking.hostName = "hexamac";

    nixpkgs.overlays = [
        (self: super: {
            python3 = super.python3.override {
                self = self.python3;
                packageOverrides = python-self: python-super: {
                    responses = (python-super.responses.overridePythonAttrs (old: {
                        doCheck = false;
                    }));
                };
            };
        })
    ];

    environment.systemPackages = with pkgs; [
        alacritty
    ];

    environment.shells = with pkgs; [ zsh bashInteractive ];

    # enable autocomplete
    environment.etc."zshenv".text = lib.mkForce ''
      # /etc/zshenv: DO NOT EDIT -- this file has been generated automatically.
      # This file is read for all shells.
      # Only execute this file once per shell.
      # But don't clobber the environment of interactive non-login children!
      if [ -n "$__ETC_ZSHENV_SOURCED" ]; then return; fi
      export __ETC_ZSHENV_SOURCED=1
      # Don't execute this file when running in a pure nix-shell.
      if test -n "$IN_NIX_SHELL"; then return; fi
      if [ -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]; then
        . ${config.system.build.setEnvironment}
      fi
      # Tell zsh how to find installed completions
      for p in ''${(z)NIX_PROFILES}; do
        fpath+=($p/share/zsh/site-functions $p/share/zsh/$ZSH_VERSION/functions $p/share/zsh/vendor-completions)
      done
      ${config.programs.zsh.shellInit}
      # Read system-wide modifications.
      if test -f /etc/zshenv.local; then
        source /etc/zshenv.local
      fi
    '';

    # https://github.com/LnL7/nix-darwin/issues/214
    system.activationScripts.applications.text = pkgs.lib.mkForce (''
        echo "setting up ~/Applications/Nix..."
        rm -rf ~/Applications/Nix
        mkdir -p ~/Applications/Nix
        chown hexular ~/Applications/Nix
        find ${config.system.build.applications}/Applications -maxdepth 1 -type l | while read f; do
            src="$(/usr/bin/stat -f%Y $f)"
            appname="$(basename $src)"
            osascript -e "tell app \"Finder\" to make alias file at POSIX file \"/Users/hexular/Applications/Nix/\" to POSIX file \"$src\" with properties {name: \"$appname\"}";
        done
    '');

    system.defaults.NSGlobalDomain = {
        InitialKeyRepeat = 44; # * 15 = 660 ms
        KeyRepeat = 2; # * 15 = 30 ms
        ApplePressAndHoldEnabled = false; # Turn off accent menu on hold
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
