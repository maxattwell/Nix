#!/usr/bin/env bash

# The workspace ID is passed as the first argument
WORKSPACE_ID=$1

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

# Set inactive icon color based on theme
if is_dark_mode; then
    INACTIVE_ICON="0xffebdbb2"  # gruvbox light fg
else
    INACTIVE_ICON="0xff3c3836"  # gruvbox dark fg
fi

# Check if this workspace is the focused one
if [ "$WORKSPACE_ID" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set "$NAME" background.drawing=on \
                             background.color="$ACTIVE_BG" \
                             background.corner_radius=9 \
                             icon.color="$ACTIVE_ICON"
else
    sketchybar --set "$NAME" background.drawing=off \
                             icon.color="$INACTIVE_ICON"
fi
