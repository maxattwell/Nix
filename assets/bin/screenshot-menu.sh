#!/usr/bin/env bash

# Screenshot and screen recording menu for Hyprland
# Uses rofi for menu selection, grim/slurp for screenshots, gpu-screen-recorder for video

SCREENSHOT_DIR="$HOME/Pictures/Screenshots"
RECORDING_DIR="$HOME/Videos/Recordings"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RECORDING_PID_FILE="/tmp/gpu-screen-recorder.pid"

# Force NVIDIA driver for gpu-screen-recorder
export __EGL_VENDOR_LIBRARY_FILENAMES=/run/opengl-driver/share/glvnd/egl_vendor.d/10_nvidia.json
export __GLX_VENDOR_LIBRARY_NAME=nvidia

# Create directories if they don't exist
mkdir -p "$SCREENSHOT_DIR"
mkdir -p "$RECORDING_DIR"

# Menu options
options=(
    "Screenshot - Area"
    "Screenshot - Window"
    "Screenshot - Fullscreen"
    "Record - Area"
    "Record - Window"
    "Stop Recording"
)

# Show rofi menu
choice=$(printf '%s\n' "${options[@]}" | rofi -dmenu -i -p "Capture" -theme-str 'window {width: 400px;}')

# Exit if user cancelled
[ -z "$choice" ] && exit 0

# Wait for rofi to disappear
sleep 0.5

# Handle selection
case "$choice" in
    "Screenshot - Area")
        slurp | grim -g - "$SCREENSHOT_DIR/screenshot_area_$TIMESTAMP.png"
        notify-send "Screenshot" "Area screenshot saved" -i camera-photo
        ;;
    
    "Screenshot - Window")
        # Get the active window geometry using hyprctl
        geometry=$(hyprctl -j activewindow | jq -r '"\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"')
        if [ -n "$geometry" ] && [ "$geometry" != "null null nullxnull" ]; then
            grim -g "$geometry" "$SCREENSHOT_DIR/screenshot_window_$TIMESTAMP.png"
            notify-send "Screenshot" "Window screenshot saved" -i camera-photo
        else
            notify-send "Screenshot" "No active window found" -u critical -i dialog-error
        fi
        ;;
    
    "Screenshot - Fullscreen")
        grim "$SCREENSHOT_DIR/screenshot_full_$TIMESTAMP.png"
        notify-send "Screenshot" "Fullscreen screenshot saved" -i camera-photo
        ;;
    
    "Record - Area")
        if [ -f "$RECORDING_PID_FILE" ]; then
            notify-send "Recording" "A recording is already in progress" -u critical -i dialog-error
        else
            region=$(slurp)
            if [ -n "$region" ]; then
                gpu-screen-recorder -w "$region" -f 60 -a default_input -o "$RECORDING_DIR/recording_area_$TIMESTAMP.mp4" > /tmp/gpu-screen-recorder.log 2>&1 &
                pid=$!
                echo $pid > "$RECORDING_PID_FILE"
                sleep 0.5
                # Check if process is still running
                if kill -0 $pid 2>/dev/null; then
                    notify-send "Recording" "Area recording started" -i media-record
                else
                    rm "$RECORDING_PID_FILE"
                    notify-send "Recording" "Failed to start recording. Check /tmp/gpu-screen-recorder.log" -u critical -i dialog-error
                fi
            fi
        fi
        ;;
    
    "Record - Window")
        if [ -f "$RECORDING_PID_FILE" ]; then
            notify-send "Recording" "A recording is already in progress" -u critical -i dialog-error
        else
            # Get the active window geometry and convert to region format
            win_info=$(hyprctl -j activewindow)
            x=$(echo "$win_info" | jq -r '.at[0]')
            y=$(echo "$win_info" | jq -r '.at[1]')
            w=$(echo "$win_info" | jq -r '.size[0]')
            h=$(echo "$win_info" | jq -r '.size[1]')
            
            if [ -n "$x" ] && [ "$x" != "null" ]; then
                region="${w}x${h}+${x}+${y}"
                gpu-screen-recorder -w "$region" -f 60 -a default_input -o "$RECORDING_DIR/recording_window_$TIMESTAMP.mp4" > /tmp/gpu-screen-recorder.log 2>&1 &
                pid=$!
                echo $pid > "$RECORDING_PID_FILE"
                sleep 0.5
                # Check if process is still running
                if kill -0 $pid 2>/dev/null; then
                    notify-send "Recording" "Window recording started" -i media-record
                else
                    rm "$RECORDING_PID_FILE"
                    notify-send "Recording" "Failed to start recording. Check /tmp/gpu-screen-recorder.log" -u critical -i dialog-error
                fi
            else
                notify-send "Recording" "No active window found" -u critical -i dialog-error
            fi
        fi
        ;;
    
    "Stop Recording")
        if [ -f "$RECORDING_PID_FILE" ]; then
            pid=$(cat "$RECORDING_PID_FILE")
            kill -INT "$pid" 2>/dev/null
            rm "$RECORDING_PID_FILE"
            notify-send "Recording" "Recording stopped and saved" -i media-playback-stop
        else
            notify-send "Recording" "No recording in progress" -u normal -i dialog-information
        fi
        ;;
    
    *)
        # User cancelled or closed menu
        exit 0
        ;;
esac
