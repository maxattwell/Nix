# hosts/macbookair/home.nix
{ config, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager/zsh.nix
  ];

  home = {
    username = "max";
    stateVersion = "24.11";
  };
}
