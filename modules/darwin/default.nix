{ config, lib, pkgs, ... }:

{
  users.users.max.home = "/Users/max";

  users.users.max.shell = pkgs.zsh;

  environment.systemPackages = with pkgs; [
    emacs-29
    brave
    iterm2
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    yarn
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-29;
  };
}
