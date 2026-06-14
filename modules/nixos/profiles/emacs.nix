{ config, lib, pkgs, ... }:

{
  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
  ];

  environment.systemPackages = with pkgs; [
    gcc
    ripgrep
    ispell
    emacs-pgtk
  ];
}
