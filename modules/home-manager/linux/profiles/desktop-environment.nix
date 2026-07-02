{ pkgs, ... }:

{
  imports = [
    ../../ghostty
    ../noctalia
  ];

  fonts.fontconfig.enable = true;

  services.gnome-keyring = {
    enable = true;
    components = [ "secrets" ];
  };

  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome-themes-extra;
    };
    gtk4.theme = null;
  };
}
