# set scrollback history to 10000
set -g history-limit 10000
set-option -g history-limit 10000

# make tmux display things in 256 colors
set -g default-terminal "screen-256color"

# Bind C-a (Ctrl+a) to default action on tmux
set-option -g prefix C-q
unbind C-b

# Use vim keys for splitting
bind s split-window -v
bind v split-window -h

# Use vim type keys for navigating between windows
bind h select-pane -L
bind l select-pane -R
bind k select-pane -U
bind j select-pane -D

# Pane Joining/Splitting
bind-key J command-prompt -p "join pane number:"  "join-pane -s '%%'"
bind-key W command-prompt -p "move pane to window:" "break-pane -d -t '%%'"

# Window switching
bind -n S-up    new-window
bind -n S-down  confirm-before -p "kill-window #W? (y/n)" kill-window
bind -n S-left  prev
bind -n S-right next

# Pane switching
bind -n C-left  select-pane -L
bind -n C-right select-pane -R
bind -n C-down  select-pane -D
bind -n C-up    select-pane -U

# Special pane operations
bind -n C-M-up    respawn-pane -k
bind -n C-M-down  kill-pane
bind -n C-M-left  swap-pane -U
bind -n C-M-right swap-pane -D

# Window splitting
bind -n C-M-\ split-window -h
bind -n C-M-] split-window -v

#
# COLORS
#

set -g status-bg black
set -g status-fg white
set -g window-status-current-bg white
set -g window-status-current-fg black
set -g window-status-current-attr bold
set -g status-interval 60
set -g status-left-length 30
set -g status-left '#[fg=colour235,bg=colour252,bold] #S #[fg=colour252,bg=colour238,nobold]#[fg=colour245,bg=colour238,bold] #(whoami) #[fg=colour238,bg=colour234,nobold]'
set -g status-right-length 115
set -g status-right "#(cat ~/Dropbox/weather.txt) | #(~/scripts/google-music.sh current) | #[fg=white]%H:%M  "

# Reload tmux
bind-key r source-file ~/.tmux.conf \; display-message "~/.tmux.conf reloaded"

# this will renumber windows automatically when one gets deleted
set-option -g renumber-windows on

# slow esc time
# https://github.com/tmux/tmux/issues/353
set -s escape-time 0

