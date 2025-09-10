{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs-30
    ispell
    ripgrep
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-30;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];
}
