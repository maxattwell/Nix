{ config, lib, pkgs, ... }:

let
  cfg = config.noctalia.hyprland;
in
{
  options.noctalia.hyprland.mod = lib.mkOption {
    type = lib.types.enum [ "SUPER" "ALT" ];
    default = "SUPER";
    description = "Hyprland main modifier for the Noctalia config.";
  };

  config = {
  xdg.configFile = {
    "hypr/hyprland.conf".text =
      builtins.replaceStrings [ "$mod = SUPER" ] [ "$mod = ${cfg.mod}" ] (builtins.readFile ./hyprland.conf);
  };

  programs.zsh.profileExtra = ''
    if [ -z "$WAYLAND_DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
      exec start-hyprland
    fi
  '';

  home.packages = with pkgs; [
    # For screenshot & record plugin
    grim
    imagemagick
    satty
    swappy
    wf-recorder
  ];
  };
}
