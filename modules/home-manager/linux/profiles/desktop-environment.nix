{ pkgs, ... }:

{
  fonts.fontconfig.enable = true;

  imports = [
    ../../ghostty
    ../noctalia
  ];

  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome-themes-extra;
    };
    gtk4.theme = null;
  };
}
