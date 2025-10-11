#!/usr/bin/env bash

# The $SELECTED variable is available for space components and indicates if
# the space invoking this script (with name: $NAME) is currently selected:
# https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item

# $SID is the space ID provided by SketchyBar
SPACE_ID="$SID"

# Theme detection for gruvbox colors
is_dark_mode() {
    [[ $(defaults read -g AppleInterfaceStyle 2>/dev/null) == "Dark" ]]
}

if is_dark_mode; then
    # Dark mode gruvbox colors
    ACTIVE_BG="0xff458588"  # gruvbox blue
    ACTIVE_ICON="0xff1d2021"  # gruvbox dark0_hard
    ICON_COLOR="0xffebdbb2"
else
    # Light mode gruvbox colors
    ACTIVE_BG="0xff076678"  # gruvbox blue (darker for light mode)
    ACTIVE_ICON="0xfffbf1c7"  # gruvbox light0
    ICON_COLOR="0xff3c3836"
fi

# Check if this space has windows using yabai
WIN=$(yabai -m query --spaces --space "$SPACE_ID" 2>/dev/null | jq '.windows[0]')
HAS_WINDOWS="false"
if [ "$WIN" != "null" ] && [ -n "$WIN" ]; then
    HAS_WINDOWS="true"
fi

# Get list of app names in this space for icons
APPS=$(yabai -m query --windows --space "$SPACE_ID" 2>/dev/null | jq -r '.[].app')

# Create icons for windows
LABEL=""
if [ "$HAS_WINDOWS" = "true" ]; then
    while IFS= read -r app; do
        if [ -n "$app" ]; then
            case "$app" in
                "iTerm2"|"iTerm")
                    LABEL+=" " ;;
                "Emacs"|"emacs")
                    LABEL+=" " ;;
                "Google Chrome"|"Chrome")
                    LABEL+=" " ;;
                "Brave Browser"|"Brave")
                    LABEL+=" " ;;
                "Notion"|"notion")
                    LABEL+=" " ;;
                "Microsoft Teams"|"teams")
                    LABEL+="󰊻 " ;;
                "Finder"|"finder")
                    LABEL+=" " ;;
                *)
                    LABEL+=" " ;;
            esac
        fi
    done <<< "$APPS"
fi

# Show this space only if it has windows OR is currently selected
if [ "$SELECTED" = "true" ] || [ "$HAS_WINDOWS" = "true" ]; then
    DRAWING="on"
else
    DRAWING="off"
fi

# Update appearance based on selection state
if [ "$SELECTED" = "true" ]; then
    sketchybar --set "$NAME" drawing="$DRAWING" \
                             background.drawing=on \
                             background.color="$ACTIVE_BG" \
                             icon.color="$ACTIVE_ICON" \
                             label="$LABEL" \
                             label.color="$ACTIVE_ICON" \
                             label.drawing="$HAS_WINDOWS"
else
    sketchybar --set "$NAME" drawing="$DRAWING" \
                             background.drawing=off \
                             icon.color="$ICON_COLOR" \
                             label="$LABEL" \
                             label.color="$ICON_COLOR" \
                             label.drawing="$HAS_WINDOWS"
fi
