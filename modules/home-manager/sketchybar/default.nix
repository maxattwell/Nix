{ config, lib, pkgs, ... }:

let
  sketchybarSource = "$HOME/Nix/modules/home-manager/sketchybar";
in
{
  programs.sketchybar = {
    enable = true;
  };

  # Use an activation hook to symlink the config file
  home.activation.linkSketchybarConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p $HOME/.config/sketchybar
    ln -sf ${sketchybarSource}/sketchybarrc $HOME/.config/sketchybar/sketchybarrc
  '';
}
