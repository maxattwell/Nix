{ config, lib, pkgs, ... }:

{
  programs.waybar.enable = true;

  # Symlink config files for easy editing without rebuilds
  # Note: style.css is managed by set-theme.sh script, not by home-manager
  xdg.configFile = {
    "waybar/config".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/waybar/config.json";
  };
}
