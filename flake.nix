{
  description = "Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";  # Pin to the stable branch

    # # nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.11-darwin";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixarr.url = "github:rasmus-kirk/nixarr";

    darwin-emacs = {
      url = "github:c4710n/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nix-darwin,
    nixarr,
    darwin-emacs,
    ...
  }@inputs: {
    nixosConfigurations = {
      shedservarr = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./hosts/shedservarr.nix
          nixarr.nixosModules.default
        ];
        specialArgs = { inherit inputs; };
      };
    };

    darwinConfigurations = {
      macbookair = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";

        modules = [ 
          ./hosts/macbookair.nix
          { nixpkgs = { overlays = [ darwin-emacs.overlays.emacs ]; }; }
          home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };
  };
}
