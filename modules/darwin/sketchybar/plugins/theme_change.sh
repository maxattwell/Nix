#!/usr/bin/env bash

# Theme detection for gruvbox colors
is_dark_mode() {
    [[ $(defaults read -g AppleInterfaceStyle 2>/dev/null) == "Dark" ]]
}

if is_dark_mode; then
    # Dark mode colors (gruvbox dark)
    ICON_COLOR="0xffebdbb2"
    LABEL_COLOR="0xffebdbb2"
    SPACE_BG_COLOR="0xff3c3836"
else
    # Light mode colors (gruvbox light)
    ICON_COLOR="0xff3c3836"
    LABEL_COLOR="0xff3c3836"
    SPACE_BG_COLOR="0xfff2e5bc"
fi

# Update all default colors
sketchybar --bar color=0x00000000

# Update all items with new colors using regex
sketchybar --set '/.*/' icon.color="$ICON_COLOR" \
                        label.color="$LABEL_COLOR"

# Update bracket backgrounds
sketchybar --set left_items background.color="$SPACE_BG_COLOR" \
           --set center_items background.color="$SPACE_BG_COLOR" \
           --set right_items background.color="$SPACE_BG_COLOR"

# Trigger aerospace workspace change to update workspace colors
sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$(aerospace list-workspaces --focused)
