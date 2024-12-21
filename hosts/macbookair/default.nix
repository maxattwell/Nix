{ config, pkgs, ... }:

{
  imports = [
    ./system.nix
    ../../modules/base.nix
    ../../modules/darwin/yabai.nix
    ../../modules/darwin/skhd.nix
  ];


  security.pam.enableSudoTouchIdAuth = true;

  services.nix-daemon.enable = true;
  system.stateVersion = 5;

}
