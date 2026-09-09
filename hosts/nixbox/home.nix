{ ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/linux/profiles/desktop-environment.nix
    ../../modules/home-manager/linux/profiles/emacs.nix
  ];

  home.stateVersion = "24.11";
}
