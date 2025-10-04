#!/bin/sh

DISK_USAGE=$(df -h /System/Volumes/Data | awk 'NR==2 {print $5}' | sed 's/%//')

if [ "$DISK_USAGE" = "" ]; then
  exit 0
fi

ICON=""

sketchybar --set "$NAME" icon="$ICON" label="${DISK_USAGE}%"
