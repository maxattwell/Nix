{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    lazygit
  ];

  # Symlink config file for easy editing without rebuilds
  xdg.configFile."lazygit/config.yml".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazygit/config.yml";
}
