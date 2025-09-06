{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/nixarr.nix
    ../../modules/nixos/homepage.nix
    ../../modules/nixos/calibre.nix
    # ../../modules/nixos/pihole.nix
    # ../../modules/nixos/kodi.nix
  ];

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
    # retroarchFull
    dualsensectl
    nvtopPackages.nvidia
    claude-code
    pass
  ];

  services.openssh.enable = true;

  # HDD config
  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/ec29d845-26e9-4367-80e7-ae743a362ecf";
    fsType = "ext4";
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
