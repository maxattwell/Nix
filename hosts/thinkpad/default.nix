{ config, pkgs, inputs, hostname, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/networkmanager.nix
    ../../modules/nixos/dns-public.nix
    ../../modules/nixos/hyprland.nix
    ../../modules/nixos/noctalia.nix
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/dev.nix
    ../../modules/nixos/remote-access.nix
  ];

  documentation.doc.enable = false;

  environment.systemPackages = with pkgs; [
    chromium
    # spotify
  ];

  hardware.bluetooth.enable = true;
  services.tuned.enable = true;
  services.upower.enable = true;

  system.stateVersion = "26.05"; # Did you read the comment?

}
