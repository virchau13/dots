{
    description = "A Home Manager flake";

    inputs = {
        nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
        darwin.url = "github:lnl7/nix-darwin/master";
        darwin.inputs.nixpkgs.follows = "nixpkgs";
        home-manager.url = github:nix-community/home-manager;
        neovim-nightly-overlay.url = github:nix-community/neovim-nightly-overlay;
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
    };

    outputs = inputs@{ self, darwin, nixpkgs, home-manager, neovim-nightly-overlay, ... }: {
        darwinConfigurations.hexamac = darwin.lib.darwinSystem {
            system = "x86_64-darwin";
            specialArgs = { inputs = inputs; };
            modules = [ 
                home-manager.darwinModule
                ./hexamac.nix
                ./common-system.nix
            ];
        };
    };
}
