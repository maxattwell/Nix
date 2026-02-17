{ config, pkgs, hostname, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/remote-access.nix
    ../../modules/nixos/nixarr.nix
    ../../modules/nixos/homepage.nix
    ../../modules/nixos/calibre-web.nix
    # ../../modules/nixos/kodi.nix
  ];

  networking.hostName = hostname;

  environment.systemPackages = with pkgs; [
    kitty.terminfo
  ];

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
