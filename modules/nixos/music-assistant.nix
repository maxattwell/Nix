{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    music-assistant-desktop
  ];

  services.music-assistant = {
    enable = true;
    providers = [
      "spotify"
      "soundcloud"
      "ytmusic"
      "radiobrowser"
    ];
  };
}
