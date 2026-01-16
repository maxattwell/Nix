#!/usr/bin/env sh

set -e

THEME="$1"
WALLPAPER_DIR="$HOME/Nix/assets/backgrounds"

WAYBAR_THEME_DIR="$HOME/Nix/modules/home-manager/waybar"
WAYBAR_CURRENT_THEME_DIR="$HOME/.config/waybar"

KITTY_THEME_DIR="$HOME/Nix/assets/kitty-themes"
KITTY_CURRENT_THEME="$HOME/.config/kitty/current_theme.conf"

SWAYNC_THEME_DIR="$HOME/.config/swaync"
SWAYNC_STYLE="$HOME/.config/swaync/style.css"

if [ "$THEME" = "light" ]; then
    emacsclient -e "(progn (mapc #'disable-theme custom-enabled-themes) (load-theme 'doom-gruvbox-light t))"
    WALLPAPER="keys-l.png"
    WAYBAR_STYLE="light.css"
    KITTY_THEME="Solarized_Light.conf"
    SWAYNC_CSS_FILE="light.css"
elif [ "$THEME" = "dark" ]; then
    emacsclient -e "(progn (mapc #'disable-theme custom-enabled-themes) (load-theme 'doom-gruvbox t))"
    WALLPAPER="keys-d.png"
    WAYBAR_STYLE="dark.css"
    KITTY_THEME="Solarized_Dark.conf"
    SWAYNC_CSS_FILE="dark.css"
else
    echo "Usage: $0 [light|dark]"
    exit 1
fi

# Set wallpapers
hyprctl hyprpaper wallpaper "DP-3,$WALLPAPER_DIR/$WALLPAPER"
hyprctl hyprpaper wallpaper "DP-4,$WALLPAPER_DIR/$WALLPAPER"

# Update Waybar style
ln -sf "$WAYBAR_THEME_DIR/$WAYBAR_STYLE" "$WAYBAR_CURRENT_THEME_DIR/style.css"

# Update Current background
ln -sf "$WALLPAPER_DIR/$WALLPAPER" "$WALLPAPER_DIR/current.png"

# Update Kitty theme
ln -sf "$KITTY_THEME_DIR/$KITTY_THEME" "$KITTY_CURRENT_THEME"

# Reload Waybar styles
pkill -SIGUSR2 waybar

# Reload Kitty
pkill -SIGUSR1 kitty

# Update SwayNC theme
ln -sf "$SWAYNC_THEME_DIR/$SWAYNC_CSS_FILE" "$SWAYNC_STYLE"

# Reload SwayNC
swaync-client -rs
