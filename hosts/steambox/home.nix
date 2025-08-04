{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
  ];

  home = {
    username = "max";
    homeDirectory = "/home/max";
    stateVersion = "24.11";
    pointerCursor = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
      size = 24;
      x11.enable = true;
      gtk.enable = true;
    };
  };

  # KDE/Plasma specific configurations
  programs.plasma = {
    enable = true;
  };

  programs.home-manager.enable = true;
}