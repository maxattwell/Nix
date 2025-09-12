#!/bin/sh

MEMORY_PRESSURE=$(memory_pressure | grep "System-wide memory free percentage" | awk '{print $5}' | sed 's/%//')
RAM_USAGE=$((100 - MEMORY_PRESSURE))

if [ "$RAM_USAGE" = "" ]; then
  exit 0
fi

ICON=""

sketchybar --set "$NAME" icon="$ICON" label="${RAM_USAGE}%"
