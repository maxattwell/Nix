{ ... }:

{
  imports = [
    ../networkmanager.nix
    ../hyprland.nix
    ../noctalia.nix
    ../thunar.nix
  ];

  hardware.bluetooth.enable = true;
}
