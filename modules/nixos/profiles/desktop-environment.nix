{ ... }:

{
  imports = [
    ../networkmanager.nix
    ../hyprland.nix
    ../noctalia.nix
    ../browser.nix
    ../thunar.nix
    ../email.nix
  ];

  services.gnome.gnome-keyring.enable = true;

  hardware.bluetooth.enable = true;
}
