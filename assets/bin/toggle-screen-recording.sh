#!/usr/bin/env bash
set -euo pipefail

videos_dir="${XDG_VIDEOS_DIR:-$HOME/Videos}"
mkdir -p "$videos_dir"

if pgrep -x gpu-screen-recorder >/dev/null 2>&1 || pgrep -f '/gpu-screen-recorder .* -o ' >/dev/null 2>&1; then
  pkill -SIGINT -x gpu-screen-recorder 2>/dev/null || pkill -SIGINT -f '/gpu-screen-recorder .* -o ' 2>/dev/null || true
  notify-send "Screen recording stopped" "Saved to $videos_dir" || true
  exit 0
fi

output="$videos_dir/recording_$(date +%Y%m%d_%H%M%S).mp4"
notify-send "Screen recording" "Select a monitor/window/area to start recording" || true

exec gpu-screen-recorder \
  -w portal \
  -f 60 \
  -k h264 \
  -ac opus \
  -a default_output \
  -q very_high \
  -cursor yes \
  -cr limited \
  -o "$output"
