{ config, pkgs, ... }:

{
  services.calibre-web = {
    enable = true;
    group = "media";
    listen = {
      ip = "0.0.0.0";
      port = 8083;
    };

    options = {
      calibreLibrary = "/data/media/library/books";
      enableBookUploading = true;
      enableBookConversion = true;
    };
  };

  # Open firewall for web access
  networking.firewall.allowedTCPPorts = [ 8083 ];

  # # Fix for calibre-web missing metadata.db (when new db location is not valid)
  # Run the following command to download and set permissions for the db
  # cd /data/media/library/books && sudo curl -L "https://github.com/janeczku/calibre-web/raw/master/library/metadata.db" -o metadata.db && sudo chown calibre-web:media metadata.db && sudo chmod 664 metadata.db
}
