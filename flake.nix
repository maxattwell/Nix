{
  description = "Max Attwell Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

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
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    nixarr = {
      url = "github:rasmus-kirk/nixarr";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    opencode-flake.url = "github:maxattwell/opencode-flake";
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
        nixbox = nixpkgs.lib.nixosSystem {
          system = linuxSystem;
          modules = [
            ./hosts/nixbox

            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.max = {
                imports = [ ./hosts/nixbox/home.nix ];
              };
            }
          ];
          specialArgs = { inherit inputs; };
        };

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
