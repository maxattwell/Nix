{ config, lib, pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    settings = {
      font_family = "JetBrainsMono Nerd Font Mono";
      font_size = "13.0";
    };
    extraConfig = ''
      confirm_os_window_close 0
      map ctrl+backspace send_text all \x1b[3;5~
    '';
  };
}
