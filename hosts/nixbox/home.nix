{ ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/linux/profiles/desktop-environment.nix
    ../../modules/home-manager/linux/profiles/emacs.nix
    ../../modules/home-manager/linux/notion
  ];

  home.stateVersion = "24.11";
}
