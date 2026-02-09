{ config, lib, pkgs, ... }:

{
  # programs.kitty.enable = true;

  # Symlink config file for easy editing without rebuilds
  xdg.configFile = {
    "kitty/kitty.conf".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/kitty/kitty.conf";
  };
}
