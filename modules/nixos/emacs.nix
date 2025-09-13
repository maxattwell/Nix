{ config, lib, pkgs, ... }:

{
  services.openssh.enable = true;

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  environment.systemPackages = with pkgs; [
    ripgrep
    ispell
    emacs
    # Latex pkgs
    (texlive.combine {
      inherit (texlive) scheme-medium
        # Quantum information packages
        braket
        physics
        # Additional useful packages
        latexmk
        dvisvgm
        dvipng;
    })
  ];
}
