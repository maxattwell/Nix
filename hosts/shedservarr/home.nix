{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
  ];


  home = {
    username = "max";
    homeDirectory = "/home/max";
    stateVersion = "24.11";
  };

  programs.home-manager.enable = true;
}
