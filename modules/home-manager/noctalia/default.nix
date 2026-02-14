{ config, lib, pkgs, ... }:

{
  xdg.configFile = {
    "hypr/hyprland.conf".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/noctalia/hyprland.conf";
  };
}
