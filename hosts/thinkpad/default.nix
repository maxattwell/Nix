{ config, pkgs, hostname, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/profiles/desktop-environment.nix
    ../../modules/nixos/dns-public.nix
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/dev.nix
    ../../modules/nixos/remote-access.nix
  ];

  documentation.doc.enable = false;

  services.tuned.enable = true;
  services.upower.enable = true;

  system.stateVersion = "26.05"; # Did you read the comment?

}
