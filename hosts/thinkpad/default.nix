{ config, pkgs, inputs, hostname, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/dev.nix
    ../../modules/nixos/remote-access.nix
  ];

  documentation.doc.enable = false;

  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.rose-pine-hyprcursor.packages.${pkgs.stdenv.hostPlatform.system}.default
    chromium
    hyprcursor
    # spotify
  ];

  hardware.bluetooth.enable = true;
  services.tuned.enable = true;
  services.upower.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "gtk"; 
  };

  networking = {
    hostName = hostname;
    networkmanager = {
      enable = true;
      dns = "none";
    };
    useNetworkd = false;
    firewall.enable = false;
    nameservers = [ "1.1.1.1" "8.8.8.8" ];

  };

  system.stateVersion = "26.05"; # Did you read the comment?

}
