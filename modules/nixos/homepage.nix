{ config, lib, pkgs, ... }:

let
  networkIP = "192.168.31.218";

  sonarrPort = 8989;
  sonarrKey = "1fb854a00ed2474ead57e9e3cc81e77d";

  radarrPort = 7878;
  radarrKey = "3f7a89922d184f8b9192dc27afb29808";

  transmissionPort = 9091;
  transmissionUsername = "max";
  transmissionPassword = "land";

  jellyfinPort = 8096;
  jellyfinKey = "00bd060650044ab098e9f945f1c79ad9";

  prowlarrPort = 9696;
  prowlarrKey = "6c6deb549af5414298eb900aad4d7c13";

in
{
  services.homepage-dashboard = {
    enable = true;
    openFirewall = true;
    settings.title = "ShedServarr";
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
