{ config, lib, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  nix.settings.experimental-features = [ "nix-command" "flakes"];

  networking.networkmanager.enable = true;

  programs.git.enable = true;
}
