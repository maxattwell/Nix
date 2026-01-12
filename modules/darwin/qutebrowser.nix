{ config, lib, pkgs, ... }:

{
  # Install qutebrowser via Homebrew on macOS (nixpkgs version has broken wayland dependency)
  homebrew.casks = [ "qutebrowser" ];

  # Configure qutebrowser for the primary user
  # macOS uses ~/.qutebrowser/ instead of ~/.config/qutebrowser/
  home-manager.users.${config.system.primaryUser} = {
    home.file.".qutebrowser/config.py".source =
      config.home-manager.users.${config.system.primaryUser}.lib.file.mkOutOfStoreSymlink 
        "${config.home-manager.users.${config.system.primaryUser}.home.homeDirectory}/Nix/modules/home-manager/qutebrowser/config.py";
  };
}
