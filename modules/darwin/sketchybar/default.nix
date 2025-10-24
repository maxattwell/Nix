{ config, pkgs, lib, ... }:

let
  username = "max";
  userHome = "/Users/${username}";
  sketchybarSourcePath = "${userHome}/Nix/modules/darwin/sketchybar";
in
{
  services.sketchybar = {
    enable = true;
    extraPackages = [ pkgs.jq ];
    # config = builtins.readFile ./sketchybarrc;
  };

  system.activationScripts.extraActivation.text = ''
    sudo -u ${username} mkdir -p ${userHome}/.config/sketchybar
    sudo -u ${username} ln -sf ${sketchybarSourcePath}/sketchybarrc ${userHome}/.config/sketchybar/sketchybarrc
  '';
}
