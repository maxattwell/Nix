{ config, lib, pkgs, ... }:

{
  programs.waybar.enable = true;

  # Symlink config files for easy editing without rebuilds
  xdg.configFile = {
    "waybar/config".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/waybar/config.json";
    "waybar/style.css".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/waybar/light.css";
  };
}
