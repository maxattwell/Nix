{ config, pkgs, ... }:

{
  imports = [
    ./system.nix
    ../../modules/nixos
    ../../modules/nixarr.nix
    ../../modules/homepage.nix
    ../../modules/kodi.nix
  ];
}
