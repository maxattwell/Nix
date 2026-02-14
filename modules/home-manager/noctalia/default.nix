{ config, lib, pkgs, ... }:

{
  xdg.configFile = {
    "hypr/hyprland.conf".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/noctalia/hyprland.conf";
  };

  programs.zsh.profileExtra = ''
    if [ -z "$WAYLAND_DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
      exec start-hyprland
    fi
  '';
}
