{ config, pkgs, ... }:

{
  imports = [
    ./system.nix
    ../../modules/nixos
    ../../modules/nixos/nixarr.nix
    ../../modules/nixos/homepage.nix
    ../../modules/nixos/kodi.nix
  ];
}
