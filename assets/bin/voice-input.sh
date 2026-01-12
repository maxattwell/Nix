#!/usr/bin/env bash

set -euo pipefail

STATE_FILE="/tmp/voice-recording-state"
TEMP_AUDIO="/tmp/voice-recording.wav"
MODEL_PATH="$HOME/whisper/ggml-base.en.bin"

# Check if currently recording
if [ -f "$STATE_FILE" ]; then
    # Stop recording
    PID=$(cat "$STATE_FILE")
    kill "$PID" 2>/dev/null || true
    rm -f "$STATE_FILE"
    
    # Wait a moment for file to be written
    sleep 0.5
    
    if [ ! -f "$TEMP_AUDIO" ] || [ ! -s "$TEMP_AUDIO" ]; then
        notify-send "❌ Voice Input" "No audio recorded"
        rm -f "$TEMP_AUDIO"
        exit 1
    fi
    
    # Notify processing
    notify-send -t 2000 "⏳ Voice Input" "Transcribing..."
    
    # Transcribe
    TRANSCRIPTION=$(whisper-cli -m "$MODEL_PATH" -f "$TEMP_AUDIO" 2>/dev/null | \
        grep '^\[' | \
        sed 's/^\[[^]]*\] *//' | \
        tr '\n' ' ' | \
        sed 's/  */ /g' | \
        sed 's/^ *//; s/ *$//')
    
    if [ -z "$TRANSCRIPTION" ]; then
        notify-send "❌ Voice Input" "No speech detected"
        rm -f "$TEMP_AUDIO"
        exit 1
    fi
    
    # Copy to clipboard
    echo "$TRANSCRIPTION" | wl-copy
    
    # Notify completion
    notify-send -t 3000 "✓ Voice Input" "Copied to clipboard:\n$TRANSCRIPTION"
    
    # Cleanup
    rm -f "$TEMP_AUDIO"
    
else
    # Start recording
    notify-send -t 2000 "🎙️ Voice Input" "Recording started\nPress hotkey again to stop"
    
    # Start recording in background and save PID
    pw-record "$TEMP_AUDIO" &
    echo $! > "$STATE_FILE"
fi
