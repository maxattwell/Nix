{
  description = "Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";  # Pin to the stable branch
    nixarr.url = "github:rasmus-kirk/nixarr";
  };

  outputs = {
    nixpkgs,
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
  };
}
