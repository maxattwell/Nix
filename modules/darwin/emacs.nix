{ config, lib, pkgs, ... }:

let
  emacs-30-patched = pkgs.emacs30.overrideAttrs (old: {
    patches = (old.patches or []) ++ [
      ./patches/emacs-30/fix-window-role.patch
      ./patches/emacs-30/system-appearance.patch
      ./patches/emacs-30/round-undecorated-frame.patch
    ];
  });
in
{
  environment.systemPackages = with pkgs; [
    emacs-30-patched
    ispell
    ripgrep
  ];

  services.emacs = {
    enable = true;
    package = emacs-30-patched;
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
