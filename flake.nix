{
    description = "dots";

    inputs = {
        darwin = {
            url = "github:lnl7/nix-darwin/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        home-manager = {
            url = github:nix-community/home-manager;
            inputs.nixpkgs.follows = "nixpkgs";
        };
        neovim-nightly-overlay = {
            url = github:nix-community/neovim-nightly-overlay;
            inputs.nixpkgs.follows = "nixpkgs";
        };
        sops-nix = {
            url = github:Mic92/sops-nix;
            inputs.nixpkgs.follows = "nixpkgs";
        };
        flake-utils.url = github:numtide/flake-utils;
        eww = {
            url = github:elkowar/eww;
        };

        # nvim plugins
        parinfer-rust = {
            url = github:virchau13/parinfer-rust/replace-cargo-sha256;
            flake = false;
        };
        yuck-vim = {
            url = github:elkowar/yuck.vim;
            flake = false;
        };

        fidget-nvim = {
            url = github:j-hui/fidget.nvim;
            flake = false;
        };

        virt-column-nvim = {
            url = github:lukas-reineke/virt-column.nvim;
            flake = false;
        };

        lsp_lines-nvim = {
            # the sourcehut input just doesnt work for some reason (it fails to download it)
            url = git+https://git.sr.ht/~whynothugo/lsp_lines.nvim?rev=22ef87c6b2039aac069bfba1c5be5cd26c27b752;
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
