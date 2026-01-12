{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    mako
  ];

  # Symlink mako config for easy theme switching
  # The active config will be managed by set-theme.sh script
  xdg.configFile = {
    "mako/dark".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/mako/dark";
    "mako/light".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/mako/light";
  };
}
