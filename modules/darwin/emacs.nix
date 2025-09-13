{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    emacs-30
    ispell
    ripgrep
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-30;
  };

  fonts.packages = with pkgs; [
    nerd-fonts.mononoki
    nerd-fonts.jetbrains-mono
    nerd-fonts.overpass
  ];

  # Homebrew configuration for latex
  homebrew = {
    enable = true;
    casks = [
      "basictex"  # Minimal LaTeX distribution for org-mode inline preview
    ];
    onActivation = {
      cleanup = "zap";
      autoUpdate = true;
      upgrade = true;
    };
  };

  # Add LaTeX binaries to system PATH
  environment.systemPath = [ "/Library/TeX/texbin" ];
}
