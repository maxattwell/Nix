# hosts/macbookair/system.nix
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs-29
    nerdfonts
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-29;
  };
}
