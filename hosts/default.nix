{ lib, inputs }:

{
  nixosConfigurations = {
    nixbox = lib.mkNixosSystem {
      hostname = "nixbox";
    };

    thinkpad = lib.mkNixosSystem {
      hostname = "thinkpad";
    };

    nixtop = lib.mkNixosSystem {
      hostname = "nixtop";
      extraModules = [
        inputs.apple-silicon.nixosModules.apple-silicon-support
      ];
    };

    shedservarr = lib.mkNixosSystem {
      hostname = "shedservarr";
      extraModules = [
        inputs.nixarr.nixosModules.default
      ];
    };
  };

  darwinConfigurations = {
    macbookair = lib.mkDarwinSystem {
      hostname = "macbookair";
      extraModules = [
        inputs.nix-homebrew.darwinModules.nix-homebrew
        {
          nix-homebrew = {
            enable = true;
            enableRosetta = true;
            user = "max";
          };
        }
      ];
    };
  };
}
