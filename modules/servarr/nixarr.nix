{ config, lib, pkgs, ... }:

{
  # Enable nixarr server
  nixarr = {
    enable = true;
    jellyfin.enable = true;
    transmission.enable = true;
    bazarr.enable = true;
    prowlarr.enable = true;
    radarr.enable = true;
    sonarr.enable = true;
    readarr.enable = true;
  };
}
