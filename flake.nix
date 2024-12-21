{
  description = "Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";  # Pin to the stable branch

    # nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.11-darwin";

    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    nixarr.url = "github:rasmus-kirk/nixarr";

    darwin-emacs = {
      url = "github:c4710n/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-darwin,
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
          {
            nixpkgs = {
              overlays = [
                darwin-emacs.overlays.emacs
              ];
            };
          }
        ];
      };
    };
  };
}
