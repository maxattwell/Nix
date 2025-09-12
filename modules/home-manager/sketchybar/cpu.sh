#!/bin/sh

CPU_USAGE=$(top -l 1 | grep "CPU usage" | awk '{print $3}' | sed 's/%//' | cut -d'.' -f1)

if [ "$CPU_USAGE" = "" ]; then
  exit 0
fi

ICON="󰍛"

sketchybar --set "$NAME" icon="$ICON" label="${CPU_USAGE}%"
