{ config, lib, pkgs, ... }:

{
  services.hyprpaper = {
    enable = true;
    settings = {
      splash = false;

      preload = [
        "/home/max/Nix/backgrounds/keys-l.png"
        "/home/max/Nix/backgrounds/keys-d.png"
        "/home/max/Nix/backgrounds/current.png"
      ];

      wallpaper = [
        "HDMI-A-2, /home/max/Nix/backgrounds/keys-l.png"
        "DP-2, /home/max/Nix/backgrounds/current.png"
        "DP-3, /home/max/Nix/backgrounds/current.png"
      ];
    };
  };
}
