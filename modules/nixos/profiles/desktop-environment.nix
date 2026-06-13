{ ... }:

{
  imports = [
    ../networkmanager.nix
    ../hyprland.nix
    ../noctalia.nix
    ../browser.nix
    ../thunar.nix
  ];

  hardware.bluetooth.enable = true;
}
