{ config, lib, pkgs, ... }:

{
  nixpkgs.config.permittedInsecurePackages = [
    "aspnetcore-runtime-6.0.36"
    "dotnet-sdk-6.0.428"
  ];

  networking.firewall.allowedTCPPorts = [ 8096 3000 ];
  networking.firewall.trustedInterfaces = [ "docker0" "br-239b11668d56" ];

  nixarr = {
    enable = true;

    jellyfin.enable = true;
    seerr.enable = true;
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
