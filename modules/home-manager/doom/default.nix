{ config, lib, pkgs, ... }:

{
  home = {
    sessionVariables = {
      DOOM_DIR = "$HOME/.config/emacs/bin";
    };
    sessionPath = [
      "$DOOM_DIR"
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
