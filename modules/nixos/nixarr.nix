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
    jellyseerr.enable = true;
    prowlarr.enable = true;
    radarr.enable = true;
    sonarr.enable = true;
    transmission = {
      enable = true;
      credentialsFile = "/var/lib/secrets/transmission/settings.json";
      extraSettings = {
          rpc-whitelist-enabled = false;
          rpc-host-whitelist-enabled = false;
          rpc-bind-address = "0.0.0.0"; 
        };
    };
  };
}
