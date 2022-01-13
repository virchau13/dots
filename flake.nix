{
    description = "dots";

    inputs = {
        nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
        darwin.url = "github:lnl7/nix-darwin/master";
        darwin.inputs.nixpkgs.follows = "nixpkgs";
        home-manager.url = github:nix-community/home-manager;
        neovim-nightly-overlay.url = github:nix-community/neovim-nightly-overlay;
        sops-nix.url = github:Mic92/sops-nix;
        flake-utils.url = github:numtide/flake-utils;

        eww.url = github:virchau13/eww;
        nerd-font-patcher = {
            url = github:ryanoasis/nerd-fonts/db026dd5c9f21b9e435c20b7627d4c27a6990420;
            flake = false;
        };
        # make-nerd-fonts.url = github:virchau13/make-nerd-fonts.nix;

        # nvim plugins
        coq_nvim = {
            url = github:ms-jpq/coq_nvim;
            flake = false;
        };
        coq_artifacts = {
            url = github:ms-jpq/coq.artifacts;
            flake = false;
        };
        coq_thirdparty = {
            url = github:ms-jpq/coq.thirdparty;
            flake = false;
        };
        parinfer-rust = {
            url = github:eraserhd/parinfer-rust;
            flake = false;
        };
        yuck-vim = {
            url = github:elkowar/yuck.vim;
            flake = false;
        };
    };

    outputs = inputs@{ self, nixpkgs, darwin, home-manager, neovim-nightly-overlay, sops-nix, flake-utils, ... }: {
        darwinConfigurations.hexamac = darwin.lib.darwinSystem {
            system = "x86_64-darwin";
            specialArgs = { inputs = inputs; };
            modules = [ 
                home-manager.darwinModule
                ./hosts/hexamac/sys.nix
                ./hosts/common/sys.nix
            ];
        };

        nixosConfigurations.hexagon = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inputs = inputs; };
            modules = [
                sops-nix.nixosModules.sops
                home-manager.nixosModule
                ./hosts/hexagon/sys.nix
                ./hosts/common/sys.nix
            ];
        };
    } // flake-utils.lib.eachDefaultSystem (system: {
        devShell = nixpkgs.legacyPackages."${system}".mkShell {
            buildInputs = let pkgs = nixpkgs.legacyPackages."${system}";  in with pkgs; [
                age
                ssh-to-age
                sops
                (writeShellScriptBin "sec" ''
                    # Edits secrets files.
                    # https://unix.stackexchange.com/questions/6463/find-searching-in-parent-directories-instead-of-subdirectories
                    find-up(){
                      path="$(pwd)"
                      while [[ "$path" != "" && ! -e "$path/$1" ]]; do
                        path="''${path%/*}"
                      done
                      echo "$path"
                    }
                    # First eval `sops.nix` to generate `.sops.yaml`.
                    # (Did you know that YAML is a superset of JSON?)
                    path="$(find-up sops.nix)"
                    echo "Found sops.nix at $path"
                    nix eval --json -f "$path"/sops.nix | ${pkgs.jq}/bin/jq > "$path"/.sops.yaml
                    # Now do the actual editing.
                    exec ${pkgs.sops}/bin/sops "$@"
                '')
            ];
        };
    });
}
