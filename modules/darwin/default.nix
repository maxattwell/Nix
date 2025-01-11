{ config, lib, pkgs, ... }:

{
  users.users.max.home = "/Users/max";

  users.users.max.shell = pkgs.zsh;

  environment.systemPackages = with pkgs; [
    emacs-29
    ripgrep
    brave
    yarn
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-29;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  system.defaults.dock.autohide = true;
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;
}
