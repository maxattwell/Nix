{ config, lib, pkgs, ... }:

{
  home = {
    sessionVariables = {
      # Doom looks for user config in DOOMDIR. Keep the CLI on PATH
      # separately; DOOM_DIR is not a Doom-recognised variable.
      DOOMDIR = "$HOME/.config/doom";
    };
    sessionPath = [
      "$HOME/.config/emacs/bin"
    ];
  };

  # Symlink config files for easy editing without rebuilds
  xdg.configFile = {
    "doom/init.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/doom/init.el";
    "doom/config.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/doom/config.el";
    "doom/packages.el".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/doom/packages.el";
  };
}
