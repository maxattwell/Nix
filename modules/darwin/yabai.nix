# modules/darwin/yabai.nix
{ config, pkgs, ... }:

{
  services.yabai = {
    enable = true;
    enableScriptingAddition = false;
    config = {
      focus_follows_mouse = "autoraise";
      mouse_follows_focus = "on";
      top_padding = 2;
      bottom_padding = 30;
      left_padding = 2;
      right_padding = 2;
      window_gap = 2;
    };

    extraConfig = ''
      yabai -m signal --add event=dock_did_retart action="sudo yabai --load-sa"
      sudo yabai --load-sa
      yabai -m space --layout bsp
      yabai -m config mouse_modifier cmd
      yabai -m config mouse_action1 resize

      yabai -m rule --add app="^Ajustes del Sistema$" manage=off

      # Force management of all workspaces
      yabai -m config --space 2 layout bsp
      yabai -m config --space 3 layout bsp
      yabai -m config --space 4 layout bsp
      yabai -m config --space 5 layout bsp
      yabai -m config --space 6 layout bsp
      yabai -m config --space 7 layout bsp
      yabai -m config --space 8 layout bsp
      yabai -m config --space 9 layout bsp
      yabai -m config --space 10 layout bsp
    '';
  };
}
