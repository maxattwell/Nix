{ config, lib, pkgs, ... }:

{
  programs.ghostty.enable = true;

  xdg.configFile = {
    "ghostty/config".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/ghostty/config";
  };
}
