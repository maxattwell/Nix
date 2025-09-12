{ config, lib, pkgs, ... }:

{
  programs.quickshell.enable = true;

  home.packages = with pkgs; [
    vlc
  ];
}
