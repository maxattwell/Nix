{ config, lib, pkgs, ... }:

{
  services.gammastep = {
    enable = true;

    # Christchurch, New Zealand coordinates
    latitude = -43.5;
    longitude = 172.6;

    # Optional: adjust color temperature (default is fine for most)
    temperature = {
      day = 6500;
      night = 3500;
    };

    settings = {
      general = {
        # Adjust gradually over this many minutes
        fade = 1;

        # Run your theme script on period change
        adjustment-method = "wayland";
      };
    };
  };

  # Create systemd service overrides to run your theme script
  systemd.user.services.gammastep = {
    Service = {
      # Set environment variables for the script
      Environment = "PATH=${pkgs.hyprland}/bin:${pkgs.coreutils}/bin:${pkgs.procps}/bin:${pkgs.emacs}/bin";

      # Create hooks using ExecStartPost/ExecStartPre
      ExecStartPost = "${pkgs.writeShellScript "gammastep-theme-check" ''
        # Wait for gammastep to determine period
        sleep 2

        # Check current period and set theme accordingly
        PERIOD=$(${pkgs.gammastep}/bin/gammastep -p 2>/dev/null | ${pkgs.gnugrep}/bin/grep "Period:" | ${pkgs.gawk}/bin/awk '{print $2}')

        if [ "$PERIOD" = "Night" ]; then
          ${pkgs.bash}/bin/bash ${config.home.homeDirectory}/Nix/assets/bin/set-theme.sh dark
        elif [ "$PERIOD" = "Daytime" ]; then
          ${pkgs.bash}/bin/bash ${config.home.homeDirectory}/Nix/assets/bin/set-theme.sh light
        fi
      ''}";
    };
  };

  # Alternative: Use a separate timer-based service for more reliable hooks
  systemd.user.services.theme-switcher = {
    Unit = {
      Description = "Automatic theme switcher based on time of day";
      After = [ "graphical-session.target" ];
    };

    Service = {
      Type = "oneshot";
      Environment = "PATH=${pkgs.hyprland}/bin:${pkgs.coreutils}/bin:${pkgs.procps}/bin:${pkgs.emacs}/bin";
      ExecStart = "${pkgs.writeShellScript "theme-switcher" ''
        # Get current period from gammastep
        PERIOD=$(${pkgs.gammastep}/bin/gammastep -p 2>/dev/null | ${pkgs.gnugrep}/bin/grep "Period:" | ${pkgs.gawk}/bin/awk '{print $2}')

        if [ "$PERIOD" = "Night" ]; then
          ${pkgs.bash}/bin/bash ${config.home.homeDirectory}/Nix/assets/bin/set-theme.sh dark
        elif [ "$PERIOD" = "Daytime" ]; then
          ${pkgs.bash}/bin/bash ${config.home.homeDirectory}/Nix/assets/bin/set-theme.sh light
        fi
      ''}";
    };
  };

  systemd.user.timers.theme-switcher = {
    Unit = {
      Description = "Check and switch theme periodically";
    };

    Timer = {
      # Check every 10 minutes
      OnCalendar = "*:0/10";
      Persistent = true;
    };

    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
