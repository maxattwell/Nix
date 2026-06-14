{ ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/nixos
    ../../modules/nixos/profiles/remote-access.nix
    ../../modules/nixos/profiles/media-server.nix
    # ../../modules/nixos/kodi.nix
  ];

  system.stateVersion = "24.05";
}
