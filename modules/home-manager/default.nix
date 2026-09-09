{ config, lib, pkgs, ... }:
{
  home = {
    username = lib.mkDefault "max";
    homeDirectory = lib.mkDefault (if pkgs.stdenv.isDarwin then "/Users/max" else "/home/max");

    sessionVariables = {
      NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      PNPM_HOME = "$HOME/.local/share/pnpm";
      # Prefer ntn's file-backed auth store. The system keyring path can report
      # a successful browser login but fail to return the token afterwards.
      NOTION_KEYRING = "0";
    };

    sessionPath = [
      "$HOME/.npm-global/bin"
      "$HOME/.local/bin"
      "$HOME/.local/share/pnpm"
      "$HOME/.bun/bin"
      "$HOME/.pi/agent/bin"
    ];

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
