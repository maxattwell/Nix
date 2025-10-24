{
  description = "Max Attwell Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixarr = {
      url = "github:rasmus-kirk/nixarr";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin-emacs = {
      url = "github:nix-giant/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-homebrew = {
      url = "github:zhaofengli-wip/nix-homebrew";
    };

    # opencode-flake.url = "github:maxattwell/opencode-flake";
  };

  outputs = inputs@{
    self,
    nixpkgs,
    home-manager,
    nix-darwin,
    darwin-emacs,
    nix-homebrew,
    nixarr,
    ...
  }:
    let
      lib = import ./lib { inherit inputs; };
    in {
      inherit lib;

      nixosConfigurations = {
        nixbox = lib.mkNixosSystem {
          hostname = "nixbox";
        };

        shedservarr = lib.mkNixosSystem {
          hostname = "shedservarr";
          extraModules = [ nixarr.nixosModules.default ];
        };
      };

      darwinConfigurations = {
        macbookair = lib.mkDarwinSystem {
          hostname = "macbookair";
          extraModules = [
            { nixpkgs.overlays = [ darwin-emacs.overlays.emacs ]; }
            nix-homebrew.darwinModules.nix-homebrew {
              nix-homebrew = {
                enable = true;
                enableRosetta = true;
                user = "max";
              };
            }
          ];
        };
      };
    };
}
