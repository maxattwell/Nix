{ config, lib, pkgs, ... }:

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
      href = "http://192.168.31.218:8989";
      description = "Series managment";
      widget = {
        type = "sonarr";
        url = "http://localhost:8989";
        key = "3ccf2999c2224299b9d14ff59351520c";
      };
    };
  }
  {
    "Radarr" = {
      icon = "radarr.png";
      href = "http://192.168.31.218:7878";
      description = "Movie managment";
      widget = {
        type = "radarr";
        url = "http://localhost:7878";
        key = "a71c72f65c7340a8b281e007c2ef9562";
      };
    };
  }
        {
    "Bazarr" = {
      icon = "bazarr.png";
      href = "http://192.168.31.218:6767";
      description = "Subtitle managment";
      widget = {
        type = "bazarr";
        url = "http://localhost:6767";
        key = "57cb3b81ffec832e4eae920382fa313e";
      };
    };
  }
  {
    "Readarr" = {
      icon = "readarr.png";
      href = "http://192.168.31.218:8787";
      description = "eBook managment";
      widget = {
        type = "readarr";
        url = "http://localhost:8787";
        key = "069c6579bf694de2a1da604a432ca92d";
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
      href = "http://192.168.31.218:9091";
      widget = {
        type = "transmission";
        url = "http://localhost:9091";
        username = "max";
        password = "land";
      };
    };
  }
  {
    "Jellyfin" = {
      icon = "jellyfin.png";
      href = "http://192.168.31.218:8096";
      description = "Stream show and movies";
      widget = {
        type = "jellyfin";
        url = "http://localhost:8096";
        key = "2125c8a6a7e84c7e9813326e002869f7";
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
      href = "http://192.168.31.218:9696";
      description = "Indexers managment";
      widget = {
        type = "prowlarr";
        url = "http://localhost:9696";
        key = "12fe0902f4eb4e4284c42138f8e096bd";
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
