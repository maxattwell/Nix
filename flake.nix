# Directory structure:
# .
# ├── flake.nix
# ├── hosts
# │   ├── macbookair
# │   │   ├── default.nix
# │   │   ├── system.nix
# │   │   └── home.nix
# │   └── shedservarr
# │       ├── default.nix
# │       └── hardware.nix
# ├── modules
# │   ├── darwin
# │   │   ├── yabai.nix
# │   │   └── skhd.nix
# │   └── home-manager
# │       └── zsh.nix

# flake.nix
{
  description = "Nix systems configuration flake";

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
      url = "github:c4710n/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixarr = {
      url = "github:rasmus-kirk/nixarr";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nix-darwin, darwin-emacs, nixarr, ... }@inputs:
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
