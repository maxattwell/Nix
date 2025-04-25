{ config, lib, pkgs, ... }:

{
    home.file.".config/waybar/style.css".source = ./light.css; # default
    home.file.".config/waybar/light.css".source = ./light.css;
    home.file.".config/waybar/dark.css".source = ./dark.css;

    programs.waybar = {
        enable = true;

        settings = {
            mainBar = {
                position = "bottom";
                height = 30;
                spacing = 20;
                modules-left = ["hyprland/workspaces"];
                modules-center = ["bluetooth" "custom/radioplayer"];
                modules-right = ["memory" "cpu" "custom/gpu" "clock"];
                "custom/radioplayer" = {
                    format = "";
                    return-type = "json";
                    on-click = "$HOME/Nix/assets/bin/play-radio.sh";
                    tooltip = false;
                };
                "hyprland/workspaces" = {
                    format = "{icon} <small>{windows}</small>";
                    window-rewrite-default = " ";
                    window-rewrite = {
                        kitty = " ";
                        chrome = " ";
                        emacs = " ";
                        GitHub = " ";
                        Outlook = " ";
                        WhatsApp = " ";
                        "class<chrome> title<*.github.*>" = " ";
                    };
                };
                bluetooth = {
                    format = "󰂯";
                    format-disabled = "󰂲";
                    format-connected = "{num_connections} ";
                    on-click = "blueman-manager";
                };
                clock = {
                    timezone = "Pacific/Auckland";
                    format = "<small>󰃭</small> {:%a %d %b     <small></small>  %I:%M  %p}";
                    tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
                };
                cpu = {
                    format = "{usage}% ";
                    tooltip = false;
                };
                memory = {
                    format = "{}% ";
                };
                "custom/gpu" = {
                    exec = "nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | awk '{print $1\"%\"}'"; 
                    format = "{} 󰢮";
                    interval = 5;
                    tooltip = false;
                };
            };
        };
    };
}