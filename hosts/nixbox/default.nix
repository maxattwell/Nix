{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/bluetooth.nix
    ../../modules/nixos/nvidia.nix
    ../../modules/nixos/sddm.nix
    ../../modules/nixos/dev.nix
  ];

  networking = {
    hostName = "nixbox";
    networkmanager.enable = true;
    useNetworkd = false;
    firewall.enable = false;
  };

  environment.etc."resolv.conf".text = ''
    nameserver 1.1.1.1
    nameserver 8.8.8.8
  '';

  # services.displayManager.ly.enable = true;

  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    gnupg

    ripgrep
    ispell
    emacs

    wofi
    slurp
    grim

    vlc
    brave
    google-chrome

    zip
    tree
    unzip
  ];

  services.openssh.enable = true;

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  virtualisation.docker.enable = true;
  users.users.max.extraGroups = [ "docker" ];

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
