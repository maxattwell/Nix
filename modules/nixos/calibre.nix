{ config, lib, pkgs, ... }:

{
  services.calibre-web = {
    enable = true;
    listen.ip = "0.0.0.0";
    listen.port = 8083;
    options = {
      enableBookUploading = true;
    };
  };

  networking.firewall.allowedTCPPorts = [ 8083 ];
}
