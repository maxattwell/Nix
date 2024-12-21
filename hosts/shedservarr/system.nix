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

}
