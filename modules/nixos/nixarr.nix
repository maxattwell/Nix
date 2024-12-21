{ config, lib, pkgs, ... }:

{
  # Needed to build sonarr in 24.11
  # nixpkgs.config.permittedInsecurePackages = [
    # "aspnetcore-runtime-6.0.36"
    # "aspnetcore-runtime-wrapped-6.0.36"
    # "dotnet-sdk-6.0.428"
    # "dotnet-sdk-wrapped-6.0.428"
  # ];
    nixpkgs.config.permittedInsecurePackages = [
      "aspnetcore-runtime-6.0.36"
      "dotnet-sdk-6.0.428"
    ];

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
