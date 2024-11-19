{ config, pkgs, ... }:

{
  imports =
    [
      ../modules/base.nix
      ../modules/locale.nix
      ../modules/hardware.nix
      ../modules/users.nix
      ../modules/servarr/hardware-configuration.nix
      ../modules/servarr/nixarr.nix
      ../modules/servarr/homepage.nix
      ../modules/servarr/kodi.nix
    ];

  networking.hostName = "shedservarr";

  # Xbox controller driver
  hardware.xpadneo.enable = true;

  environment.systemPackages = with pkgs; [
    neovim
    fastfetch
    bluez
    linuxConsoleTools
    wget
    retroarchFull
    moonlight-qt
    dualsensectl
  ];

  services.openssh.enable = true;

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
