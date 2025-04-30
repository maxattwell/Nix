{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    hypridle
  ];

  xdg.configFile."hypr/hypridle.conf".text = ''
    $LOCKSCREEN = pidof hyprlock || hyprlock

    general {
        lock_cmd = $LOCKSCREEN
        before_sleep_cmd = $LOCKSCREEN    # command ran before sleep
    }

    # Lock it first before dpms off so that screen won't show for a moment after wake up.
    listener {
        timeout = 600
        on-timeout = $LOCKSCREEN
    }

    # Suspend
    listener {
        timeout = 900
        on-timeout = systemctl suspend
    }
  '';
}
