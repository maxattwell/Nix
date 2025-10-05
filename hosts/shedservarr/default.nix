{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/nixarr.nix
    ../../modules/nixos/homepage.nix
    ../../modules/nixos/calibre-web.nix
    # ../../modules/nixos/kodi.nix
  ];

  networking.hostName = "shedservarr";

  services.openssh.enable = true;

  # HDD config
  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/ec29d845-26e9-4367-80e7-ae743a362ecf";
    fsType = "ext4";
  };

  # HDD power management - multiple approaches for reliability
  services.udev.extraRules = ''
    ACTION=="add|change", SUBSYSTEM=="block", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", RUN+="${pkgs.hdparm}/bin/hdparm -B 90 -S 41 /dev/%k"
  '';

  # Backup method: Apply hdparm settings at boot and via systemd
  powerManagement = {
    enable = true;
    powerUpCommands = ''
      # Wait for drives to be ready
      sleep 5
      # Apply to all SATA drives
      for drive in /dev/sd[a-z]; do
        if [ -e "$drive" ]; then
          ${pkgs.hdparm}/bin/hdparm -B 90 -S 41 "$drive" || true
        fi
      done
    '';
  };

  # Also try via systemd service
  systemd.services.hdparm-power-management = {
    description = "Set HDD power management";
    wantedBy = [ "multi-user.target" ];
    after = [ "local-fs.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      sleep 10  # Give drives time to initialize
      for drive in /dev/sd[a-z]; do
        if [ -e "$drive" ] && [ -b "$drive" ]; then
          echo "Configuring power management for $drive"
          ${pkgs.hdparm}/bin/hdparm -B 90 -S 41 "$drive" || echo "Failed to configure $drive"
          ${pkgs.hdparm}/bin/hdparm -C "$drive" || echo "Failed to check $drive status"
        fi
      done
    '';
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
