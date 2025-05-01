{ config, lib, pkgs, ... }:

{
  imports = [
    ./hyprland.nix
    ./hyprpaper.nix
    ./hyprlock.nix
  ];
}
