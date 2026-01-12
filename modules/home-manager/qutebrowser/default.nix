{ config, lib, pkgs, ... }:

{
  programs.qutebrowser = {
    enable = true;
  };

  xdg.configFile."qutebrowser/config.py".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/qutebrowser/config.py";
}
