{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    swaynotificationcenter
  ];

  # Symlink swaync config/style for easy theme switching
  xdg.configFile = {
    "swaync/config.json".source = ./config.json;
    "swaync/style.css".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/swaync/light.css";
  };
}
