{ pkgs, ... }:

{
  programs.hyprland = {
    enable = true;
    # Start the session through uwsm so graphical-session.target (and
    # therefore xdg-desktop-portal) is properly activated. Launch with:
    #   uwsm start hyprland
    withUWSM = true;
  };

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
