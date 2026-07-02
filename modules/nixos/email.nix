{ pkgs, ... }:

{
  programs.evolution = {
    enable = true;
    plugins = [ pkgs.evolution-ews ]; # needed for Exchange/M365
  };

}
