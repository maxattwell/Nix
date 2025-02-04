{
  description = "Max Attwell Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin-emacs = {
      # pin to a commit so we dont have to rebuild emacs on each flake update
      url = "github:c4710n/nix-darwin-emacs/25949adae777a2718eb5c66e0a5b034408ce6e63";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    nixarr = {
      url = "github:rasmus-kirk/nixarr";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nix-darwin,
    darwin-emacs,
    nix-homebrew,
    nixarr,
    ...
  }@inputs:
    let
      darwinSystem = "aarch64-darwin";
      linuxSystem = "x86_64-linux";
    in {
      nixosConfigurations = {
        shedservarr = nixpkgs.lib.nixosSystem {
          system = linuxSystem;
          modules = [
            ./hosts/shedservarr

            nixarr.nixosModules.default

            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.max = import ./hosts/shedservarr/home.nix;
            }
          ];
          specialArgs = { inherit inputs; };
        };
      };

      darwinConfigurations = {
        macbookair = nix-darwin.lib.darwinSystem {
          system = darwinSystem;
          modules = [
            ./hosts/macbookair

            { nixpkgs.overlays = [ darwin-emacs.overlays.emacs ]; }

            nix-homebrew.darwinModules.nix-homebrew {
              nix-homebrew = {
                enable = true;
                enableRosetta = true;
                user = "max";
              };
            }

            home-manager.darwinModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.max = import ./hosts/macbookair/home.nix;
            }
          ];
          specialArgs = { inherit inputs; };
        };
      };
    };
}
