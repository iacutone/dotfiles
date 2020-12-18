#!/usr/bin/env bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$CURRENT_DIR/scripts/helpers.sh"

artist="#($CURRENT_DIR/scripts/artist.sh)"
album="#($CURRENT_DIR/scripts/album.sh)"
track="#($CURRENT_DIR/scripts/track.sh)"
music_status="#($CURRENT_DIR/scripts/status.sh)"

artist_interpolation="\#{artist}"
album_interpolation="\#{album}"
track_interpolation="\#{track}"
status_interpolation="\#{music_status}"

#Backwards compatibility
spotify_artist=artist
spotify_album=album
spotify_track=track
spotify_status=music_status

spotify_artist_interpolation=artist_interpolation
spotify_album_interpolation=album_interpolation
spotify_track_interpolation=track_interpolation
spotify_status_interpolation=status_interpolation

do_interpolation() {
  local output="$1"
  local output="${output/$artist_interpolation/$artist}"
  local output="${output/$album_interpolation/$album}"
  local output="${output/$track_interpolation/$track}"
  local output="${output/$status_interpolation/$music_status}"
  echo "$output"
}

update_tmux_uptime() {
  local option="$1"
  local option_value="$(get_tmux_option "$option")"
  local new_option_value="$(do_interpolation "$option_value")"
  set_tmux_option "$option" "$new_option_value"
}

main() {
  update_tmux_uptime "status-right"
  update_tmux_uptime "status-left"
}
main
