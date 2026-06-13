{ ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/linux/profiles/desktop-environment.nix
    ../../modules/home-manager/linux/profiles/emacs.nix
  ];

  home.stateVersion = "26.05";

  noctalia.hyprland.mod = "ALT";
}
