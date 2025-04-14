{ config, lib, pkgs, ... }:

let
  theme = "light";
  waybarStyle = pkgs.writeText "style.css" (builtins.readFile ./${theme}.css);
in {
  programs.waybar = {
    enable = true;

    style = waybarStyle;

    settings = {
      mainBar = {
        position = "bottom";
        height = 30;
        spacing = 20;
        modules-left = ["hyprland/workspaces"];
        modules-center = ["bluetooth" "custom/radioplayer"];
        modules-right = ["memory" "cpu" "clock"];
        "custom/radioplayer" = {
            format = "";
            return-type = "json";
            on-click = "$HOME/Nix/assets/bin/play-radio.sh";
            tooltip = false;
        };
        "hyprland/workspaces" = {
            format = "{icon} <small>{windows}</small>";
            window-rewrite-default = " ";
            window-rewrite = {
                kitty = " ";
                chrome = " ";
                emacs = " ";
                GitHub = " ";
                Outlook = " ";
                WhatsApp = " ";
                "class<chrome> title<*.github.*>" = " ";
            };
        };
        bluetooth = {
            format = "󰂯";
            format-disabled = "󰂲";
            format-connected = "{num_connections} ";
            on-click = "blueman-manager";
        };
        clock = {
            timezone = "Pacific/Auckland";
            format = "<small>󰃭</small> {:%a %d %b     <small></small>  %I:%M  %p}";
            tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        };
        cpu = {
            format = "{usage}% ";
            tooltip = false;
        };
        memory = {
            format = "{}% ";
        };
      };
    };
  };
}
