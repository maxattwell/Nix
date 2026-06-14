{ ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/profiles/desktop-environment.nix
    ../../modules/nixos/profiles/laptop.nix
    ../../modules/nixos/dns-public.nix
    ../../modules/nixos/profiles/emacs.nix
    ../../modules/nixos/profiles/dev.nix
    ../../modules/nixos/profiles/insurgence.nix
    ../../modules/nixos/profiles/remote-access.nix
  ];

  system.stateVersion = "26.05"; # Did you read the comment?

}
