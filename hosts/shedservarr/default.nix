{ config, pkgs, ... }:

{
  imports = [
    ./system.nix
    ../../modules/nixos
    ../../modules/nixos/nixarr.nix
    ../../modules/nixos/homepage.nix
    ../../modules/nixos/kodi.nix
  ];

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
