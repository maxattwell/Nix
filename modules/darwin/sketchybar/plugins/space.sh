#!/bin/sh

# The $SELECTED variable is available for space components and indicates if
# the space invoking this script (with name: $NAME) is currently selected:
# https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item

# Theme detection for gruvbox colors
is_dark_mode() {
    [[ $(defaults read -g AppleInterfaceStyle 2>/dev/null) == "Dark" ]]
}

if is_dark_mode; then
    # Dark mode gruvbox colors
    ACTIVE_BG="0xff458588"  # gruvbox blue
    ACTIVE_ICON="0xff1d2021"  # gruvbox dark0_hard
else
    # Light mode gruvbox colors  
    ACTIVE_BG="0xff076678"  # gruvbox blue (darker for light mode)
    ACTIVE_ICON="0xfffbf1c7"  # gruvbox light0
fi

if [ "$SELECTED" = "true" ]; then
    sketchybar --set "$NAME" background.drawing=on \
                             background.color="$ACTIVE_BG" \
                             icon.color="$ACTIVE_ICON"
else
    sketchybar --set "$NAME" background.drawing=off \
                             icon.color=0xffebdbb2
fi
