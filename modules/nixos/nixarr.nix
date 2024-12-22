{ config, lib, pkgs, ... }:

{
  nixpkgs.config.permittedInsecurePackages = [
    "aspnetcore-runtime-6.0.36"
    "dotnet-sdk-6.0.428"
  ];

  # Enable nixarr server
  nixarr = {
    enable = true;
    jellyfin.enable = true;
    transmission.enable = true;
    prowlarr.enable = true;
    radarr.enable = true;
    sonarr.enable = true;
  };
}
