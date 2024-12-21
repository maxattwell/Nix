# hosts/macbookair/home.nix
{ config, pkgs, ... }:

{
  # imports = [
  #   ../../modules/home-manager/zsh.nix
  # ];

  home = {
    username = "max";
    homeDirectory = "/Users/max";
    stateVersion = "25.05";
  };


}
