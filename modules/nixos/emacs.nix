{ config, lib, pkgs, ... }:

{
  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  environment.systemPackages = with pkgs; [
    ripgrep
    ispell
    emacs-pgtk
  ];
}
