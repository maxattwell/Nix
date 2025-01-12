{ config, pkgs, ... }:

{
  imports = [
    ../../modules
    ../../modules/darwin
    ../../modules/darwin/yabai.nix
    ../../modules/darwin/skhd.nix
  ];

  security.pam.enableSudoTouchIdAuth = true;

  services.nix-daemon.enable = true;
  system.stateVersion = 5;
}
