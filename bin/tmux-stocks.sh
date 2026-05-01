#!/usr/bin/env bash
CACHE="$HOME/.cache/tmux-stocks"
TTL=300

if [[ -f "$CACHE" ]]; then
    age=$(( $(date +%s) - $(stat -f %m "$CACHE") ))
    if [[ $age -lt $TTL ]]; then
        cat "$CACHE"
        exit 0
    fi
fi

tickers=("CARS" "VOO")
output=""

for ticker in "${tickers[@]}"; do
    resp=$(curl -sf --max-time 10 \
        -H "User-Agent: Mozilla/5.0 (compatible; tmux-statusbar)" \
        -H "Accept: application/json" \
        "https://query1.finance.yahoo.com/v8/finance/chart/${ticker}?range=1d&interval=1d")
    [[ -z "$resp" ]] && continue

    price=$(printf '%s' "$resp" | jq -r '.chart.result[0].meta.regularMarketPrice // empty')
    prev=$(printf '%s' "$resp" | jq -r '.chart.result[0].meta.chartPreviousClose // empty')

    [[ -z "$price" || -z "$prev" ]] && continue

    change=$(awk "BEGIN { printf \"%.1f\", ($price - $prev) / $prev * 100 }")
    if awk "BEGIN { exit ($price >= $prev) ? 0 : 1 }"; then
        arrow="↑"
    else
        arrow="↓"
        change="${change#-}"
    fi

    [[ -n "$output" ]] && output+=" | "
    output+="${ticker} ${price} ${arrow}${change}%"
done

if [[ -n "$output" ]]; then
    printf '%s' "$output" > "$CACHE"
    cat "$CACHE"
fi
