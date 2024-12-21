{ config, pkgs, ... }:

{
  users.users.max.home = "/Users/max";

  environment.systemPackages = with pkgs; [
    emacs-29
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-29;
  };
}
