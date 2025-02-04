{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName  = "Max Attwell";
    userEmail = "max.attwell@hotmail.com";
    extraConfig.init.defaultBranch = "main";
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
}
