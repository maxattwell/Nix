{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/nixarr.nix
    ../../modules/nixos/homepage.nix
    # ../../modules/nixos/kodi.nix
  ];

  networking.hostName = "shedservarr";

  services.openssh.enable = true;

  # HDD config
  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/ec29d845-26e9-4367-80e7-ae743a362ecf";
    fsType = "ext4";
  };

  # HDD power management - spin down after ~3.5 minutes of inactivity
  services.udev.extraRules = ''
    ACTION=="add|change", SUBSYSTEM=="block", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", RUN+="${pkgs.hdparm}/bin/hdparm -B 90 -S 41 /dev/%k"
  '';

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
