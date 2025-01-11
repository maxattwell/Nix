{ config, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager/zsh.nix
  ];

  home = {
    username = "max";
    homeDirectory = "/Users/max";
    stateVersion = "24.11";
  };

  programs.zsh.initExtra = ''
      # nvm
      export NVM_DIR="$HOME/.nvm"
      [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
      [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


      # pnpm
      export PNPM_HOME="/Users/max/Library/pnpm"
      case ":$PATH:" in
        *":$PNPM_HOME:"*) ;;
        *) export PATH="$PNPM_HOME:$PATH" ;;
      esac
      # pnpm end


      export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home

      export ANDROID_HOME=/Users/max/Library/Android/sdk
      export PATH=$PATH:$ANDROID_HOME/emulator
      export PATH=$PATH:$ANDROID_HOME/platform-tools
    '';

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
