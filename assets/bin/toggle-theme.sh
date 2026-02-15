#!/usr/bin/env sh

# Get the current color scheme
CURRENT_SCHEME=$(dconf read /org/gnome/desktop/interface/color-scheme)

if [ "$CURRENT_SCHEME" = "'prefer-dark'" ]; then
    # Switch to LIGHT
    noctalia-shell ipc call wallpaper set Nix/assets/backgrounds/keys-l.png eDP-1
    dconf write /org/gnome/desktop/interface/color-scheme "'default'"
    dconf write /org/gnome/desktop/interface/gtk-theme "'Adwaita'"
else
    # Switch to DARK
    noctalia-shell ipc call wallpaper set Nix/assets/backgrounds/keys-d.png eDP-1
    dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
    dconf write /org/gnome/desktop/interface/gtk-theme "'Adwaita-dark'"
fi
