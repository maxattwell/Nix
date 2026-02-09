{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/doom
    ../../modules/home-manager/ambxst
    ../../modules/home-manager/kitty
  ];

  home = {
    username = "max";
    homeDirectory = "/home/max";
    stateVersion = "24.11";
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
    client.enable = true;
    socketActivation.enable = true;
  };

  programs.home-manager.enable = true;
}
