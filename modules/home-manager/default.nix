{ config, lib, pkgs, ... }:
{
  home = {
    username = lib.mkDefault "max";
    homeDirectory = lib.mkDefault (if pkgs.stdenv.isDarwin then "/Users/max" else "/home/max");
  };

  imports = [
    ./git.nix
    ./zsh.nix
    ./tmux.nix
  ];

  programs = {
    home-manager.enable = true;

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
