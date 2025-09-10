{ config, pkgs, ... }:

{
  imports = [
    ../../modules/darwin
    ../../modules/darwin/yabai.nix
    ../../modules/darwin/skhd.nix
    ../../modules/darwin/emacs.nix
  ];

  security.pam.services.sudo_local.touchIdAuth = true;

  system.stateVersion = 5;
}
