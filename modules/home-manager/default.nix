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
      rebuild = "sudo nixos-rebuild switch --flake $HOME/Nix --impure";
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

    # Highlight selected item in completion menu
    zstyle ':completion:*' menu select

    if [[ -n "$IN_NIX_SHELL" ]]; then
      RPROMPT="%F{cyan}[nix-dev]%f"
    fi
    '';
  };

  programs.bat.enable = true;
}
