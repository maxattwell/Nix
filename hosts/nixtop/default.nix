{ config, pkgs, inputs, hostname, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/remote-access.nix
  ];

  services.upower.enable = true;
  
  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.rose-pine-hyprcursor.packages.${pkgs.system}.default
    chromium
    hyprcursor
  ];

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "gtk"; 
  };

  networking = {
    hostName = hostname;
    networkmanager.enable = true;
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?
}
