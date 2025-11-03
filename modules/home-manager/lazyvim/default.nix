{ config, lib, pkgs, ... }:

{
  home = {
    sessionVariables = {
      NVIM_DIR = "$HOME/.config/nvim";
    };
  };

  # Symlink entire nvim config directory for easy editing and to allow LazyVim to create new files
  xdg.configFile."nvim".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim";
}
