{ config, lib, pkgs, ... }:
{
  imports = [
    ./git.nix
    ./zsh.nix
    ./tmux.nix
  ];

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    bat.enable = true;

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };
  };
}
