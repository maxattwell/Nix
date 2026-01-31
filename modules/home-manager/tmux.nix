{ config, lib, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    sensibleOnTop = true;
    # extraConfig = ''
    #   set-option -g status-style bg=default
    # '';
    # plugins = with pkgs; [
    #     tmuxPlugins.dotbar
    # ];
  };
}
