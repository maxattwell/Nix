{ pkgs, ... }:

{
  programs.hyprland.enable = true;

  services.dbus.enable = true;

  security.polkit.enable = true;

  environment.systemPackages = with pkgs; [
    hyprcursor
    hyprpolkitagent
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
      xdg-desktop-portal-gtk
    ];
    config = {
      common.default = [ "hyprland" "gtk" ];
      hyprland.default = [ "hyprland" "gtk" ];
    };
  };
}
