# hosts/macbookair/default.nix
{ config, pkgs, ... }:

{
  imports = [
    ./system.nix
    ../../modules/darwin/yabai.nix
    ../../modules/darwin/skhd.nix
  ];

  nix.settings.experimental-features = "nix-command flakes";
  security.pam.enableSudoTouchIdAuth = true;

  services.nix-daemon.enable = true;
  system.stateVersion = 5;

}
