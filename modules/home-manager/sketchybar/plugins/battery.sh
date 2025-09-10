#!/bin/bash

PERCENTAGE="$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)"
CHARGING="$(pmset -g batt | grep 'AC Power')"

if [ "$PERCENTAGE" = "" ]; then
  exit 0
fi

BATTERY_COLOR=0xffa89984

if [[ "$CHARGING" != "" ]]; then
  ICON="󰂄"
  BATTERY_COLOR=0xff8ec07c
elif [[ "$PERCENTAGE" -gt 75 ]]; then
  ICON="󰁹"
elif [[ "$PERCENTAGE" -gt 50 ]]; then
  ICON="󰂀"
elif [[ "$PERCENTAGE" -gt 25 ]]; then
  ICON="󰁾"
elif [[ "$PERCENTAGE" -gt 10 ]]; then
  ICON="󰁻"
  BATTERY_COLOR=0xffd79921
else
  ICON="󰁺"
  BATTERY_COLOR=0xffcc241d
fi

sketchybar --set battery icon="$ICON" label="${PERCENTAGE}%" icon.color="$BATTERY_COLOR"