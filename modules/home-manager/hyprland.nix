{ config, lib, pkgs, ... }:

{
  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.settings = {
    exec-once = ["hyprpaper" "waybar"];

    monitor = [
      "DP-4, 1920x1080@60, 0x475, 1"
      "DP-3, 1920x1080@60, 1920x0, 1, transform, 1"
    ];

    workspace = [
      "1,monitor:DP-4"
      "2,monitor:DP-3"
      "w[tv1], gapsout:0, gapsin:0"
      "f[1], gapsout:0, gapsin:0"
    ];

    windowrule = [
      "bordersize 0, floating:0, onworkspace:w[tv1]"
      "rounding 0, floating:0, onworkspace:w[tv1]"
      "bordersize 0, floating:0, onworkspace:f[1]"
      "rounding 0, floating:0, onworkspace:f[1]"
    ];

    windowrulev2 = [
      "float, center, title:^(Bluetooth Devices)$"
    ];

    "$mod" = "SUPER";
    "$terminal" = "exec, kitty";
    "$browser" = "exec, brave";
    "$editor" = "exec, emacsclient -nc";
    "$suspend" = "exec, systemctl suspend";
    "$launcher" = "exec, wofi --show drun --style=$HOME/Nix/assets/wofi_styles.css";
    "$lighttheme" = "$HOME/Nix/assets/bin/set-theme.sh light";
    "$darktheme" = "$HOME/Nix/assets/bin/set-theme.sh dark";

    bind = [
      "$mod, RETURN, $terminal"
      "$mod SHIFT, RETURN, $browser"
      "$mod, Q, killactive,"
      "$mod, E, $editor"
      "$mod, ESCAPE, $suspend"
      "$mod SHIFT, ESCAPE, exit"
      "$mod, SPACE, $launcher"
      "$mod, F1, exec, $darktheme"
      "$mod, F2, exec, $lighttheme"

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
