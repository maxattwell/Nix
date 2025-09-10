{ config, pkgs, ... }:

{
  imports = [
    ../../modules/darwin
    ../../modules/darwin/yabai.nix
    ../../modules/darwin/skhd.nix
    ../../modules/darwin/emacs.nix
    ../../modules/darwin/dev.nix
  ];

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 5;
}
