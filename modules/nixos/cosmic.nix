{ config, lib, pkgs, inputs, ... }:
let 
    cosmic-bg-theme = inputs.cosmic-applets-collection.packages.${pkgs.system}.cosmic-ext-bg-theme;
in
{
  services.displayManager.cosmic-greeter.enable = true;
  services.desktopManager.cosmic.enable = true;

  environment.systemPackages = with pkgs; [
    cosmic-ext-applet-minimon
    inputs.cosmic-applets-collection.packages.${pkgs.system}.cosmic-ext-applet-clipboard-manager
    inputs.radio-applet.packages.${pkgs.system}.default
  ];

  fonts.packages = with pkgs; [
    comfortaa
  ];

  systemd.user.services.cosmic-ext-bg-theme = {
    description = "COSMIC Background Theme Extension";
    documentation = [ "man:cosmic-ext-bg-theme(1)" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
        Type = "simple";
        ExecStart = "${cosmic-bg-theme}/bin/cosmic-ext-bg-theme";
        Restart = "on-failure";
    };
  };
}
