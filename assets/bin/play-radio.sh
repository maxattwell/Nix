# #!/bin/bash

# Define an associative array of radio stations (key: station name, value: station URL)
declare -A stations=(
    ["George FM"]="https://digitalstreams.mediaworks.nz/george_net/playlist.m3u8"
    ["Loca FM"]="https://s3.we4stream.com:2020/stream/locafm"
    ["RNE Radio 5"]="https://rtvelivestream.akamaized.net/rtvesec/rne/rne_r5_madrid_main.m3u8"
    ["RNE Exterior"]="https://rtvelivestream.akamaized.net/rtvesec/rne/rne_re_main.m3u8"
    ["Stop all stations"]="stop"
)

# Get only the station names (keys) for wofi to display
station_names=$(printf "%s\n" "${!stations[@]}")

# Use wofi to display the list of station names and capture the selected station
selection=$(printf '%s\n' "$station_names" | wofi --dmenu --style=/home/max/dotfiles/wofi/wofi_styles.css --prompt "Select a radio station")

# Get the URL associated with the selected station
station_url="${stations[$selection]}"

# Stop any running VLC instance
if [[ "$station_url" == "stop" ]]; then
    pkill vlc
else
    # Start the selected station unless "Stop all stations" was selected
    if [[ -n "$station_url" ]]; then
        pkill vlc
        cvlc "$station_url" &
    fi
fi
