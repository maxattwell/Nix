{ pkgs, ... }:

{
  programs.hyprland.enable = true;

  security.polkit.enable = true;

  environment.systemPackages = with pkgs; [
    hyprcursor
    hyprpolkitagent
  ];

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "gtk";
  };
}
