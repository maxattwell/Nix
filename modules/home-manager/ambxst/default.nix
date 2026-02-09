{ config, inputs, pkgs, ... }: {
  home.packages = [
    inputs.ambxst.packages.${pkgs.system}.default
    pkgs.spotify
  ];


  xdg.configFile = {
    "hypr/hyprland.conf".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/ambxst/hyprland.conf";
  };
}
