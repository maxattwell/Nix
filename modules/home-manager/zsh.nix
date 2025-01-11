# modules/home-manager/zsh.nix
{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    history.size = 10000;
  };


  programs.zsh.initExtra = ''
    alias ls="ls --color"

    export DOOM_DIR=$HOME/.config/emacs/bin
    export PATH=$PATH:$DOOM_DIR
  '';
}
