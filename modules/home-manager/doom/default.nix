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

  home.file.".config/doom" = {
    source = ./.;
  };
}
