{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    hypridle
  ];

  xdg.configFile."hypr/hypridle.conf".text = ''
    general {
        lock_cmd = pidof hyprlock || hyprlock       # avoid starting multiple hyprlock instances.
        before_sleep_cmd = loginctl lock-session    # lock before suspend.
    }

    # Screenlock
    listener {
        # HYPRLOCK TIMEOUT
        timeout = 600
        # HYPRLOCK ONTIMEOUT
        on-timeout = loginctl lock-session
    }


    # Suspend
    listener {
        # SUSPEND TIMEOUT
        timeout = 900
        #SUSPEND ONTIMEOUT
        on-timeout = systemctl suspend
    }
  '';
}
