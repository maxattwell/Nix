{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/doom
    ../../modules/home-manager/ghostty
    ../../modules/home-manager/noctalia
  ];

  home = {
    username = "max";
    homeDirectory = "/home/max";
    stateVersion = "26.05";
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    startWithUserSession = "graphical";
    defaultEditor = true;
    client.enable = true;
    socketActivation.enable = true;
  };

  noctalia.hyprland.mod = "ALT";

  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark"; # Or your preferred theme
      package = pkgs.gnome-themes-extra;
    };
    gtk4.theme = null;
  };

  programs.home-manager.enable = true;
}
