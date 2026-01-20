{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/hypr
    ../../modules/home-manager/waybar
    ../../modules/home-manager/swaync
    ../../modules/home-manager/kitty
    ../../modules/home-manager/doom
    ../../modules/home-manager/lazyvim
    ../../modules/home-manager/lazygit
    ../../modules/home-manager/neovide
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

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client.enable = true;
    socketActivation.enable = true;
  };

  programs.home-manager.enable = true;
}
