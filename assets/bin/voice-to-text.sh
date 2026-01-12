#!/usr/bin/env bash

# voice-to-text.sh - Record audio and transcribe with whisper
# Usage: voice-to-text.sh [output_mode]
# output_mode: stdout (default), clipboard, or file

set -euo pipefail

OUTPUT_MODE="${1:-stdout}"
TEMP_AUDIO="/tmp/voice-recording-$$.wav"
MODEL_PATH="$HOME/whisper/ggml-base.en.bin"

# Cleanup on exit
trap 'rm -f "$TEMP_AUDIO"' EXIT

echo "Recording... Press Ctrl+C to stop" >&2

# Record audio (pw-record stops on SIGINT)
pw-record "$TEMP_AUDIO" 2>/dev/null || true

if [ ! -f "$TEMP_AUDIO" ]; then
    echo "Error: No audio recorded" >&2
    exit 1
fi

echo "Transcribing..." >&2

# Transcribe and extract just the text (remove timestamps)
TRANSCRIPTION=$(whisper-cli -m "$MODEL_PATH" -f "$TEMP_AUDIO" 2>/dev/null | \
    grep '^\[' | \
    sed 's/^\[[^]]*\] *//' | \
    tr '\n' ' ' | \
    sed 's/  */ /g' | \
    sed 's/^ *//; s/ *$//')

case "$OUTPUT_MODE" in
    clipboard)
        echo "$TRANSCRIPTION" | wl-copy
        echo "Copied to clipboard!" >&2
        ;;
    file)
        OUTPUT_FILE="${2:-transcription.txt}"
        echo "$TRANSCRIPTION" > "$OUTPUT_FILE"
        echo "Saved to $OUTPUT_FILE" >&2
        ;;
    stdout|*)
        echo "$TRANSCRIPTION"
        ;;
esac
