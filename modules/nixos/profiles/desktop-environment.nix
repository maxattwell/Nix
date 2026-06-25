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

  hardware.bluetooth.enable = true;
}
