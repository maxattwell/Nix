{ config, pkgs, ... }:

{
  services.sketchybar = {
    enable = true;
    extraPackages = [ pkgs.jq ];
    config = builtins.readFile ./sketchybarrc;
  };
}
