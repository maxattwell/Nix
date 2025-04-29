{ config, lib, pkgs, ... }:

{
  services.hyprpaper = {
    enable = true;
    settings = {
      splash = false;

      preload = [
        "$HOME/Nix/assets/backgrounds/keys-l.png"
        "$HOME/Nix/assets/backgrounds/keys-d.png"
        "$HOME/Nix/assets/backgrounds/current.png"
      ];

      wallpaper = [
        "HDMI-A-2, $HOME/Nix/assets/backgrounds/current.png"
        "DP-4, $HOME/Nix/assets/backgrounds/current.png"
        "DP-3, $HOME/Nix/assets/backgrounds/current.png"
      ];
    };
  };
}
