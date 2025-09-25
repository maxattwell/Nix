{ config, lib, pkgs, ... }:

{
  nixpkgs.config.permittedInsecurePackages = [
    "aspnetcore-runtime-6.0.36"
    "dotnet-sdk-6.0.428"
  ];

  # Enable nixarr server
  nixarr = {
    enable = true;
    # Move state/databases to SSD to allow HDD to spin down
    stateDir = "/var/lib/nixarr";  # This will be on your SSD (/ filesystem)
    mediaDir = "/data/media";      # Keep media on HDD

    jellyfin.enable = true;
    jellyseerr.enable = true;
    transmission.enable = true;
    prowlarr.enable = true;
    radarr.enable = true;
    sonarr.enable = true;
    readarr.enable = true;
  };
}
