{ config, pkgs, inputs, hostname, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/nvidia.nix
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/dev.nix
    ../../modules/nixos/remote-access.nix
  ];

  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.rose-pine-hyprcursor.packages.${pkgs.system}.default
    chromium
    hyprcursor

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

  # Swap configuration
  swapDevices = [{
    device = "/swapfile";
    size = 16384; # 16GB swap file
  }];

  zramSwap = {
    enable = true;
    algorithm = "zstd"; # High compression ratio, great for i7 CPUs
    memoryPercent = 50;  # Uses up to 12GB of your 24GB for compressed swap
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
