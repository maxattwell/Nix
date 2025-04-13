{ config, lib, pkgs, ... }:

{
  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.settings = {
    exec-once = ["hyprpaper" "waybar"];

    "$mod" = "SUPER";

    bind = [
      "$mod, RETURN, exec, kitty"
      "$mod SHIFT, RETURN, exec, brave"
      "$mod, Q, killactive,"
      "$mod, E, exec, emacsclient -nc"
      "$mod, ESCAPE, exec, systemctl suspend"
      "$mod SHIFT, ESCAPE, exit"

      "$mod, h, movefocus, l"
      "$mod, j, movefocus, d"
      "$mod, k, movefocus, u"
      "$mod, l, movefocus, r"

      "$mod SHIFT, h, movewindow, l"
      "$mod SHIFT, j, movewindow, d"
      "$mod SHIFT, k, movewindow, u"
      "$mod SHIFT, l, movewindow, r"

      "$mod, m, togglespecialworkspace, magic"
      "$mod SHIFT, m, movetoworkspace, special:magic"

      "$mod, p, workspace, previous"

      "$mod, 1, workspace, 1"
      "$mod, 2, workspace, 2"
      "$mod, 3, workspace, 3"
      "$mod, 4, workspace, 4"
      "$mod, 5, workspace, 5"
      "$mod, 6, workspace, 6"
      "$mod, 7, workspace, 7"
      "$mod, 8, workspace, 8"
      "$mod, 9, workspace, 9"
      "$mod, 0, workspace, 10"

      "$mod SHIFT, 1, movetoworkspace, 1"
      "$mod SHIFT, 2, movetoworkspace, 2"
      "$mod SHIFT, 3, movetoworkspace, 3"
      "$mod SHIFT, 4, movetoworkspace, 4"
      "$mod SHIFT, 5, movetoworkspace, 5"
      "$mod SHIFT, 6, movetoworkspace, 6"
      "$mod SHIFT, 7, movetoworkspace, 7"
      "$mod SHIFT, 8, movetoworkspace, 8"
      "$mod SHIFT, 9, movetoworkspace, 9"
      "$mod SHIFT, 0, movetoworkspace, 10"
    ];

    bindm = [
      "$mod, mouse:272, movewindow"
      "$mod, mouse:273, resizewindow"
    ];

    general = {
      gaps_in = 3;
      gaps_out = 5;
      "col.active_border" = "rgba(ffffffff)";
      "col.inactive_border" = "rgba(595959aa)";
    };

    decoration = {
      rounding = 3;
    };

    animations = {
      enabled = "yes";
      bezier = "myBezier, 0.2, 0, 0.2, 1";
      animation = [
        "windows, 1, 3, myBezier"
        "windowsOut, 1, 4, default, popin 80%"
        "border, 1, 1, default"
        "borderangle, 1, 8, default"
        "fade, 1, 7, default"
        "workspaces, 1, 6, default"
      ];
    };
  };
}
