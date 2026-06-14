{ config, pkgs, hostname, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/profiles/desktop-environment.nix
    ../../modules/nixos/nvidia.nix
    ../../modules/nixos/profiles/emacs.nix
    ../../modules/nixos/profiles/dev.nix
    ../../modules/nixos/profiles/insurgence.nix
    ../../modules/nixos/remote-access.nix
  ];

  # Linux 6.18 currently blackscreens on this GTX 1070 with the
  # supported legacy_580 NVIDIA driver, so stay on the 6.12 LTS line.
  boot.kernelPackages = pkgs.linuxPackages_6_12;

  services.flatpak.enable = true;

  services.resolved.enable = true;
  networking.networkmanager.dns = "systemd-resolved";

  environment.etc."gai.conf".text = ''
    precedence ::ffff:0:0/96 100
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
