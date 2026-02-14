{
  description = "Max Attwell Nix systems configuration flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hyprland.url = "github:hyprwm/hyprland?ref=v0.36.0";

    apple-silicon = {
      url = "github:nix-community/nixos-apple-silicon";
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

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rose-pine-hyprcursor = {
      url = "github:ndom91/rose-pine-hyprcursor";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hyprlang.follows = "hyprland/hyprlang";
    };

    radio-applet = {
      url = "github:maxattwell/cosmic-ext-applet-radio";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # cosmic-applets-collection.url = "git+file:///home/max/code/cosmic/ext-cosmic-applets-flake";

    nixarr = {
      url = "github:rasmus-kirk/nixarr";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs = inputs@{
    self,
    nixpkgs,
    apple-silicon,
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

        nixtop = lib.mkNixosSystem {
          hostname = "nixtop";
          extraModules = [ apple-silicon.nixosModules.default ];
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
