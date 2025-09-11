{ config, lib, pkgs, ... }:

let
  kittyThemes = ../../assets/kitty-themes;
in
{
  home.file.".config/kitty/current_theme.conf".source = "${kittyThemes}/Solarized_Light.conf";

  programs.kitty = {
    enable = true;
    settings = {
      font_family = "JetBrainsMono Nerd Font Mono";
      font_size = "13.0";
      background_opacity = "0.9";
      background = "#383838";
    };
    extraConfig = ''
      confirm_os_window_close 0
      map ctrl+backspace send_text all \x1b[3;5~
      # include ~/.config/kitty/current_theme.conf
    '';
  };
}
