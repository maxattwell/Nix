{ config, pkgs, inputs, hostname, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/networkmanager.nix
    ../../modules/nixos/hyprland.nix
    ../../modules/nixos/noctalia.nix
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/remote-access.nix
  ];

  services.upower.enable = true;
  
  environment.systemPackages = with pkgs; [
    chromium
  ];

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?
}
