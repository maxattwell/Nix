{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "shedservarr";

  # nvidia drivers
  hardware.nvidia.open = false;
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.bluetooth.enable = true;

  environment.systemPackages = with pkgs; [
    git
    neovim
    fastfetch
    nitch
    bluez
    linuxConsoleTools
    wget
    retroarchFull
    moonlight-qt
    dualsensectl
    nvtopPackages.nvidia
  ];

  services.openssh.enable = true;

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
