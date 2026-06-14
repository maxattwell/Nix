{ config, pkgs, hostname, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/nix-cache.nix
    ../../modules/nixos
    ../../modules/nixos/profiles/desktop-environment.nix
    ../../modules/nixos/profiles/laptop.nix
    ../../modules/nixos/dns-public.nix
    ../../modules/nixos/emacs.nix
    ../../modules/nixos/profiles/dev.nix
    ../../modules/nixos/profiles/insurgence.nix
    ../../modules/nixos/remote-access.nix
  ];

  documentation.doc.enable = false;

  system.stateVersion = "26.05"; # Did you read the comment?

}
