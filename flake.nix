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

    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";

    darwin-emacs = {
      url = "github:nix-giant/nix-darwin-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ambxst.url = "github:Axenide/Ambxst";

    radio-applet.url = "github:maxattwell/cosmic-ext-applet-radio";

    cosmic-applets-collection.url = "git+file:///home/max/code/cosmic/ext-cosmic-applets-flake";

    nixarr = {
      url = "github:rasmus-kirk/nixarr";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{
    self,
    nixpkgs,
    home-manager,
    nix-darwin,
    nix-homebrew,
    radio-applet,
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
