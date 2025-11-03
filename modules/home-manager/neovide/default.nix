{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    neovide
  ];

  # Symlink config file for easy editing without rebuilds
  xdg.configFile."neovide/config.toml".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/neovide/config.toml";
}
