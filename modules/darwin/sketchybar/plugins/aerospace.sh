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
    PREV_BG="0xff665c54"  # gruvbox gray (more obvious)
else
    # Light mode gruvbox colors
    ACTIVE_BG="0xff076678"  # gruvbox blue (darker for light mode)
    ACTIVE_ICON="0xfffbf1c7"  # gruvbox light0
    PREV_BG="0xffc9b99a"  # gruvbox light gray (more obvious)
fi

# Set inactive icon color based on theme
if is_dark_mode; then
    INACTIVE_ICON="0xffebdbb2"  # gruvbox light fg
else
    INACTIVE_ICON="0xff3c3836"  # gruvbox dark fg
fi

# Get list of app names in this workspace
APPS=$(aerospace list-windows --workspace "$WORKSPACE_ID" --format '%{app-name}' 2>/dev/null)

# Create icons/dots for windows
LABEL=""
WINDOW_COUNT=0
if [ -n "$APPS" ]; then
    while IFS= read -r app; do
        if [ -n "$app" ]; then
            ((WINDOW_COUNT++))
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

# Only show label if there are windows
if [ "$WINDOW_COUNT" -gt 0 ]; then
    LABEL_DRAWING=on
else
    LABEL_DRAWING=off
fi

# Hide the workspace entirely if it has no windows and is not focused or previous
if [ "$WINDOW_COUNT" -eq 0 ] && [ "$WORKSPACE_ID" != "$FOCUSED_WORKSPACE" ] && [ "$WORKSPACE_ID" != "$PREV_WORKSPACE" ]; then
    sketchybar --set "$NAME" icon.drawing=off \
                             label.drawing=off \
                             background.drawing=off \
                             padding_left=0 \
                             padding_right=0 \
                             icon.padding_left=0 \
                             icon.padding_right=0
    exit 0
fi

# Check if this workspace is the focused one or the previous one
if [ "$WORKSPACE_ID" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set "$NAME" icon.drawing=on \
                             background.drawing=on \
                             background.color="$ACTIVE_BG" \
                             background.corner_radius=9 \
                             icon.color="$ACTIVE_ICON" \
                             label="$LABEL" \
                             label.color="$ACTIVE_ICON" \
                             label.drawing="$LABEL_DRAWING" \
                             padding_right=2 \
                             icon.padding_left=8 \
                             icon.padding_right=8
elif [ "$WORKSPACE_ID" = "$PREV_WORKSPACE" ]; then
    sketchybar --set "$NAME" icon.drawing=on \
                             background.drawing=on \
                             background.color="$PREV_BG" \
                             background.corner_radius=9 \
                             icon.color="$INACTIVE_ICON" \
                             label="$LABEL" \
                             label.color="$INACTIVE_ICON" \
                             label.drawing="$LABEL_DRAWING" \
                             padding_right=2 \
                             icon.padding_left=8 \
                             icon.padding_right=8
else
    sketchybar --set "$NAME" icon.drawing=on \
                             background.drawing=off \
                             icon.color="$INACTIVE_ICON" \
                             label="$LABEL" \
                             label.color="$INACTIVE_ICON" \
                             label.drawing="$LABEL_DRAWING" \
                             padding_right=2 \
                             icon.padding_left=8 \
                             icon.padding_right=8
fi
