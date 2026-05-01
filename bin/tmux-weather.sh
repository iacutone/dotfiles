#!/usr/bin/env bash
CACHE="$HOME/.cache/tmux-weather"
TTL=1800

if [[ -f "$CACHE" ]]; then
    age=$(( $(date +%s) - $(stat -f %m "$CACHE") ))
    if [[ $age -lt $TTL ]]; then
        cat "$CACHE"
        exit 0
    fi
fi

resp=$(curl -sf --max-time 5 \
    "https://api.open-meteo.com/v1/forecast?latitude=28.53834&longitude=-81.37924&current=temperature_2m,weather_code&temperature_unit=fahrenheit&format=json")

if [[ -n "$resp" ]]; then
    temp=$(printf '%s' "$resp" | jq -r '.current.temperature_2m')
    if [[ -n "$temp" && "$temp" != "null" ]]; then
        printf '%s°F' "$temp" > "$CACHE"
        cat "$CACHE"
    fi
fi
