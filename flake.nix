{
  description = "Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";  # Pin to the stable branch

    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
    # nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    nixarr.url = "github:rasmus-kirk/nixarr";
  };

  outputs = {
    self,
    nixpkgs,
    nix-darwin,
    nixarr,
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
        modules = [ ./hosts/macbookair.nix ];

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;
      };
    };
  };
}
