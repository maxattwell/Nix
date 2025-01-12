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

  programs.git = {
    enable = true;
    userName  = "Max Attwell";
    userEmail = "max.attwell@hotmail.com";
    delta = {
      enable = true;
      options = {
        line-numbers = true;
      };
    };
  };

  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    history.size = 10000;
    shellAliases = {
      cat = "bat";
    };
  };

  programs.bat.enable = true;

  home.file.".config/doom" = {
    source = ./doom;
  };
}
