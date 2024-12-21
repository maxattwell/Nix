{ config, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager/zsh.nix
  ];

  home = {
    username = "max";
    homeDirectory = "/Users/max";
    stateVersion = "24.11";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
