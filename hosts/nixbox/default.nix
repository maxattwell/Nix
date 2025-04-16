{ config, pkgs, inputs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
  ];

  networking = {
    hostName = "nixbox";
    networkmanager.enable = true;
    firewall.enable = false;
  };

  hardware.nvidia.open = false;
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.bluetooth.enable = true;

  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    git
    zip
    neovim
    nitch
    bluez
    blueman
    nvtopPackages.nvidia
    kitty
    brave
    emacs
    pass
    gnupg
    ripgrep
    nodejs_22
    wofi
    vlc
    code-cursor
    aider-chat
  ];

  services.getty.autologinUser = "max";

  services.openssh.enable = true;

  services.emacs.enable = true;

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
