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
    home.sessionVariables.NOCTALIA_HYPR_MOD = cfg.mod;

    programs.zsh.profileExtra = ''
      export NOCTALIA_HYPR_MOD="${cfg.mod}"

      if [ -z "$WAYLAND_DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
        exec start-hyprland -- --config "$HOME/Nix/modules/home-manager/linux/noctalia/hyprland.lua"
      fi
    '';
  };
}
