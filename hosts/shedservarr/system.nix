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
    nitch
    bluez
    linuxConsoleTools
    wget
    retroarchFull
    dualsensectl
    nvtopPackages.nvidia
  ];

  services.openssh.enable = true;

  # HDD config
  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/ec29d845-26e9-4367-80e7-ae743a362ecf";
    fsType = "ext4";
  };
}
