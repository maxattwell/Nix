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
      rebuild = "sudo nixos-rebuild switch --flake $HOME/Nix";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "agkozak/zsh-z"; }
      ];
    };
    initContent = ''
    bindkey '^k' history-search-backward
    bindkey '^j' history-search-forward
    bindkey '^h' backward-word
    bindkey '^l' forward-word
    bindkey '^[[3;5~' backward-kill-word
    '';
  };

  programs.bat.enable = true;
}
