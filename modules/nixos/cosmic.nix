{ config, lib, pkgs, ... }:

{
  services.displayManager.cosmic-greeter.enable = true;
  services.desktopManager.cosmic.enable = true;

  environment.systemPackages = with pkgs; [
    cosmic-ext-applet-minimon
  ];

  fonts.packages = with pkgs; [
    comfortaa
  ];
}
