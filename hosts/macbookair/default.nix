{ config, pkgs, hostname, ... }:

{
  imports = [
    ../../modules/darwin
    ../../modules/darwin/yabai.nix
    ../../modules/darwin/skhd.nix
    ../../modules/darwin/emacs.nix
    ../../modules/darwin/dev.nix
    ../../modules/darwin/qutebrowser.nix
    # ../../modules/darwin/aerospace.nix
    ../../modules/darwin/sketchybar
    ../../modules/darwin/remote-access.nix
  ];

  networking.hostName = hostname;

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 5;
}
