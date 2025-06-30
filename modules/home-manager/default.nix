{ config, lib, pkgs, ... }:
{
  programs = {

    git = {
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

    zsh = {
      enable = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      enableCompletion = true;
      history.size = 10000;
      shellAliases = {
        cat = "bat";
      };
      zplug = {
        enable = true;
        plugins = [
          { name = "agkozak/zsh-z"; }
        ];
      };

      initContent = ''
      bindkey '^k' history-search-backward
      #bindkey '^j' history-search-forward
      bindkey '^h' backward-word
      bindkey '^l' forward-word
      bindkey '^[[3;5~' backward-kill-word
      # Highlight selected item in completion menu
      zstyle ':completion:*' menu select

      # Function to update rprompt
      update_rprompt() {
        rprompt_parts=""
        if [[ -n "$IN_NIX_SHELL" ]]; then
          rprompt_parts+="%F{cyan}[nix-dev]%f "
        fi
        if [[ -n "$DIRENV_DIR" ]]; then
          rprompt_parts+="%F{yellow}[direnv]%f "
        fi
        RPROMPT="$rprompt_parts"
      }

      # Update rprompt on directory change and before each command
      autoload -U add-zsh-hook
      add-zsh-hook chpwd update_rprompt
      add-zsh-hook precmd update_rprompt

      # Initial update
      update_rprompt

      # Function to update rprompt
      update_rprompt() {
        rprompt_parts=""
        if [[ -n "$IN_NIX_SHELL" ]]; then
          rprompt_parts+="%F{cyan}[nix-dev]%f "
        fi
        if [[ -n "$DIRENV_DIR" ]]; then
          rprompt_parts+="%F{yellow}[direnv]%f "
        fi
        RPROMPT="$rprompt_parts"
      }

      # Custom rebuild function
      rebuild() {
        if [[ "$(uname -s)" == "Darwin" ]]; then
          sudo darwin-rebuild switch --flake $HOME/Nix
          echo "Rebuild complete, reloading yabai..."
          sudo yabai --load-sa && echo "Yabai reload success"
        else
          # sudo nixos-rebuild switch --flake $HOME/Nix --impure
          nh os switch $HOME/Nix -- --impure
        fi
      }

      # Set npm global config
      export PATH=~/.npm-global/bin:$PATH
      '';
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    bat.enable = true;
  };
}
