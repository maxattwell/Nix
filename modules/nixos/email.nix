{ pkgs, ... }:

{
  programs.evolution = {
    enable = true;
    plugins = [ pkgs.evolution-ews ]; # needed for Exchange/M365
  };

  services.gnome.gnome-keyring.enable = true;
  security.pam.services.login.enableGnomeKeyring = true;
}
