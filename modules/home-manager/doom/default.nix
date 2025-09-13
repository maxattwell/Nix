{ config, lib, pkgs, ... }:

let
  doomSource = "$HOME/Nix/modules/home-manager/doom";
in
{
  home = {
    sessionVariables = {
      DOOM_DIR = "$HOME/.config/emacs/bin";
    };
    sessionPath = [
      "$DOOM_DIR"
    ];
  };

  # Use an activation hook to symlink directly to working files
  home.activation.linkDoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p $HOME/.config/doom

    ln -sf ${doomSource}/init.el $HOME/.config/doom/init.el
    ln -sf ${doomSource}/config.el $HOME/.config/doom/config.el
    ln -sf ${doomSource}/packages.el $HOME/.config/doom/packages.el
  '';

  # for Latex
  home.packages = with pkgs; [
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
    # zathura  # Lightweight PDF viewer
  ];
}
