#!/usr/bin/env sh

set -e

THEME="$1"
WALLPAPER_DIR="$HOME/Nix/assets/backgrounds"
WAYBAR_DIR="$HOME/.config/waybar"

if [ "$THEME" = "light" ]; then
    emacsclient -e "(progn (mapc #'disable-theme custom-enabled-themes) (load-theme 'doom-gruvbox-light t))"
    WALLPAPER="keys-l.png"
    WAYBAR_STYLE="light.css"
elif [ "$THEME" = "dark" ]; then
    emacsclient -e "(progn (mapc #'disable-theme custom-enabled-themes) (load-theme 'doom-gruvbox t))"
    WALLPAPER="keys-d.png"
    WAYBAR_STYLE="dark.css"
else
    echo "Usage: $0 [light|dark]"
    exit 1
fi

# Set wallpapers
hyprctl hyprpaper wallpaper "DP-3,$WALLPAPER_DIR/$WALLPAPER"
hyprctl hyprpaper wallpaper "DP-4,$WALLPAPER_DIR/$WALLPAPER"

# Update Waybar style
ln -sf "$WAYBAR_DIR/$WAYBAR_STYLE" "$WAYBAR_DIR/style.css"

# Update Current background
ln -sf "$WALLPAPER_DIR/$WALLPAPER" "$WALLPAPER_DIR/current.png"

# Reload Waybar styles
pkill -SIGUSR2 waybar
