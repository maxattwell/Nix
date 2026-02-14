{ config, pkgs, inputs, hostname, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/remote-access.nix
  ];

  services.displayManager.cosmic-greeter.enable = true;

  services.upower.enable = true;
  
  programs.hyprland.enable = true;

  environment.systemPackages = with pkgs; [
    inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.rose-pine-hyprcursor.packages.${pkgs.system}.default
    chromium
    hyprcursor

  ];

  nix.settings = {
    extra-substituters = [
      "https://nixos-apple-silicon.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nixos-apple-silicon.cachix.org-1:8psDu5SA5dAD7qA0zMy5UT292TxeEPzIz8VVEr2Js20="
    ];
  };
      
  networking = {
    hostName = hostname;
    networkmanager.enable = true;
  };

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?
}
