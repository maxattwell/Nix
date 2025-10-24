{ config, lib, pkgs, ... }:

{
  wayland.windowManager.hyprland.enable = true;

  programs.rofi = {
    enable = true;
    theme = "${config.home.homeDirectory}/Nix/modules/home-manager/rofi/gruvbox.rasi";
    extraConfig = {
      modi = "drun,run,window";
      show-icons = true;
      click-to-exit = true;
      hover-select = true;
      me-select-entry = "";
      me-accept-entry = "MousePrimary";
    };
  };

  home.packages = with pkgs; [
    hyprpaper
    slurp
    grim
  ];

  home.sessionVariables = {
    "WLR_NO_HARDWARE_CURSORS" = "1";
    "LIBVA_DRIVER_NAME" = "nvidia";
    "__GLX_VENDOR_LIBRARY_NAME" = "nvidia";
    "GBM_BACKEND" = "nvidia-drm";
    "__GL_VRR_ALLOWED" = 1;
    "WLR_RENDERER" = "vulkan";
  };

  # Symlink config files for easy editing without rebuilds
  xdg.configFile = {
    "hypr/hyprland.conf".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/hypr/hyprland.conf";
    "hypr/hyprpaper.conf".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/hypr/hyprpaper.conf";
  };
}
