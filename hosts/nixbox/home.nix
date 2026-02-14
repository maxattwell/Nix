{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/doom
    ../../modules/home-manager/kitty
    ../../modules/home-manager/noctalia
  ];

  home = {
    username = "max";
    homeDirectory = "/home/max";
    stateVersion = "24.11";
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    startWithUserSession = "graphical";
    defaultEditor = true;
    client.enable = true;
    socketActivation.enable = true;
  };

  programs.home-manager.enable = true;
}
