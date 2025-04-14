{ config, lib, pkgs, ... }:

{
  home = {
    sessionVariables = {
      DOOM_DIR = "$HOME/.config/emacs/bin";
    };
    sessionPath = [
      "$DOOM_DIR"
    ];
  };

  home.file.".config/doom/init.el".source = ./init.el;
  home.file.".config/doom/packages.el".source = ./packages.el;
  home.file.".config/doom/config.el".source = ./config.el;
}
