{ inputs }:

let
  darwinSystem = "aarch64-darwin";
  linuxSystem = "x86_64-linux";

  # Helper function to configure Home Manager for a user
  mkHomeManager = { homeConfig }: {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.max = homeConfig;
  };
in
{
  inherit mkHomeManager;

  # Helper to build NixOS systems
  mkNixosSystem = { hostname, extraModules ? [] }: inputs.nixpkgs.lib.nixosSystem {
    system = linuxSystem;
    modules = [
      ../hosts/${hostname}
      inputs.home-manager.nixosModules.home-manager
      (mkHomeManager { homeConfig = import ../hosts/${hostname}/home.nix; })
    ] ++ extraModules;
    specialArgs = { inherit inputs hostname; };
  };

  # Helper to build Darwin systems
  mkDarwinSystem = { hostname, extraModules ? [] }: inputs.nix-darwin.lib.darwinSystem {
    system = darwinSystem;
    modules = [
      ../hosts/${hostname}
      inputs.home-manager.darwinModules.home-manager
      (mkHomeManager { homeConfig = import ../hosts/${hostname}/home.nix; })
    ] ++ extraModules;
    specialArgs = { inherit inputs hostname; };
  };
}
