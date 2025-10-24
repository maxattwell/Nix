{ config, pkgs, inputs, hostname, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/bluetooth.nix
    ../../modules/nixos/nvidia.nix
    ../../modules/nixos/sddm.nix
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/dev.nix
  ];

  networking = {
    hostName = hostname;
    networkmanager.enable = true;
    useNetworkd = false;
    firewall.enable = false;
  };

  environment.etc."resolv.conf".text = ''
    nameserver 1.1.1.1
    nameserver 8.8.8.8
  '';

  # services.displayManager.ly.enable = true;

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
