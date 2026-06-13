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

  documentation.doc.enable = false;

  # Linux 6.18 currently blackscreens on this GTX 1070 with the
  # supported legacy_580 NVIDIA driver, so stay on the 6.12 LTS line.
  boot.kernelPackages = pkgs.linuxPackages_6_12;

  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.rose-pine-hyprcursor.packages.${pkgs.stdenv.hostPlatform.system}.default
    chromium
    hyprcursor
    # spotify
  ];

  hardware.bluetooth.enable = true;

  services.flatpak.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "gtk"; 
  };

  networking = {
    networkmanager.enable = true;
    useNetworkd = false;
    firewall.enable = false;
  };

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
