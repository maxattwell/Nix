{ config, pkgs, hostname, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/remote-access.nix
    ../../modules/nixos/nixarr.nix
    ../../modules/nixos/homepage.nix
    ../../modules/nixos/calibre-web.nix
    # ../../modules/nixos/kodi.nix
  ];

  networking.hostName = hostname;
  networking.nameservers = [ "8.8.8.8" "1.1.1.1"];

  environment.systemPackages = with pkgs; [
    ghostty.terminfo
  ];

  virtualisation.docker = {
    enable = true;
    daemon.settings = {
      dns = [ "8.8.8.8" "1.1.1.1" ];
    };
  };

  systemd.services.docker.environment = {
    GODEBUG = "netdns=cgo";
  };

  users.users.max.extraGroups = [ "docker" ];

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
