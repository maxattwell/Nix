{ config, lib, pkgs, ... }:

let
  networkIP = "shedservarr";

  sonarrPort = 8989;
  sonarrKey = "{{HOMEPAGE_VAR_SONARR_KEY}}";

  radarrPort = 7878;
  radarrKey = "{{HOMEPAGE_VAR_RADARR_KEY}}";

  jellySeerrPort = 5055;
  jellySeerrKey = "{{HOMEPAGE_VAR_JELLYSEERR_KEY}}";

  calibrePort = 8083;
  calibreUsername = "admin";
  calibrePassword = "{{HOMEPAGE_VAR_CALIBRE_PASSWORD}}";

  transmissionPort = 9091;
  transmissionUsername = "max";
  transmissionPassword = "{{HOMEPAGE_VAR_TRANSMISSION_PASSWORD}}";

  jellyfinPort = 8096;
  jellyfinKey = "{{HOMEPAGE_VAR_JELLYFIN_KEY}}";

  prowlarrPort = 9696;
  prowlarrKey = "{{HOMEPAGE_VAR_PROWLARR_KEY}}";
in
{
  services.homepage-dashboard = {
    enable = true;
    environmentFile = "/etc/homepage.env";
    openFirewall = true;
    settings.title = "ShedServarr";
    allowedHosts = "shedservarr:8082";
    services = [
      {
        "Manage" = [
          {
            "Sonarr" = {
              icon = "sonarr.png";
              href = "http://${networkIP}:${toString sonarrPort}";
              description = "Series management";
              widget = {
                type = "sonarr";
                url = "http://localhost:${toString sonarrPort}";
                key = sonarrKey;
              };
            };
          }
          {
            "Radarr" = {
              icon = "radarr.png";
              href = "http://${networkIP}:${toString radarrPort}";
              description = "Movie management";
              widget = {
                type = "radarr";
                url = "http://localhost:${toString radarrPort}";
                key = radarrKey;
              };
            };
          }
          {
            "Jellyseerr" = {
              icon = "jellyseerr.png";
              href = "http://${networkIP}:${toString jellySeerrPort}";
              description = "Media Requests";
              widget = {
                type = "jellyseerr";
                url = "http://localhost:${toString jellySeerrPort}";
                key = jellySeerrKey;
              };
            };
          }
          {
            "Calibre" = {
              icon = "calibre.png";
              href = "http://${networkIP}:${toString calibrePort}";
              description = "Book Library";
              widget = {
                type = "calibreweb";
                url = "http://localhost:${toString calibrePort}";
                username = calibreUsername;
                password = calibrePassword;

              };
            };
          }
        ];
      }
      {
        "Monitor" = [
          {
            "Transmission" = {
              icon = "transmission.png";
              href = "http://${networkIP}:${toString transmissionPort}";
              widget = {
                type = "transmission";
                url = "http://localhost:${toString transmissionPort}";
                username = transmissionUsername;
                password = transmissionPassword;
              };
            };
          }
          {
            "Jellyfin" = {
              icon = "jellyfin.png";
              href = "http://${networkIP}:${toString jellyfinPort}";
              description = "Stream show and movies";
              widget = {
                type = "jellyfin";
                url = "http://localhost:${toString jellyfinPort}";
                key = jellyfinKey;
                enableBlocks = true;
                enableNowPlaying = true;
                enableUser = true;
                showEpisodeNumber = true;
                expandOneStreamToTwoRows = true;
              };
            };
          }
          {
            "Prowlarr" = {
              icon = "prowlarr.png";
              href = "http://${networkIP}:${toString prowlarrPort}";
              description = "Indexers management";
              widget = {
                type = "prowlarr";
                url = "http://localhost:${toString prowlarrPort}";
                key = prowlarrKey;
              };
            };
          }
        ];
      }
    ];
    widgets = [
      {
        resources = {
          cpu = true;
          disk = "/";
          memory = true;
          cputemp = true;
          uptime = true;
        };
      }
    ];
  };
}
