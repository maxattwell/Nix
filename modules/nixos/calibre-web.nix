{ config, pkgs, ... }:

{
  services.calibre-web = {
    enable = true;
    listen = {
      ip = "0.0.0.0";
      port = 8083;
    };

    options = {
      calibreLibrary = "/data/calibre-library";
      enableBookUploading = true;
      enableBookConversion = true;
    };
  };

  # Open firewall for web access
  networking.firewall.allowedTCPPorts = [ 8083 ];

  # Ensure the library directory exists with proper permissions
  systemd.tmpfiles.rules = [
    "d /data/calibre-library 0755 calibre-web calibre-web -"
  ];
}
