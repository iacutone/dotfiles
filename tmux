#
set -g status-right-length 60
set -g status-right "#[fg=blue]#S #I:#P #[fg=yellow]:: %d %b %Y "

# use UTF8
set -g utf8
set-window-option -g utf8 on
set -g status-interval 2

# set scrollback history to 10000 (10k)
set -g history-limit 10000

# make tmux display things in 256 colors
set -g default-terminal "screen-256color"


# Bind C-a (Ctrl+a) to default action on tmux
set-option -g prefix C-a
unbind C-b

# Use vim keys for splitting
bind s split-window -v
bind v split-window -h

# Use vim type keys for navigating between windows
bind h select-pane -L
bind l select-pane -R
bind k select-pane -U
bind j select-pane -D

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
# set -g status-left '#[fg=green](#S) #(whoami)'
set -g status-right-length 60
set -g status-right "#(~/scripts/weather-forcast.sh) | #[fg=white]%H:%M  "

# Reload tmux
bind-key r source-file ~/.tmux.conf \; display-message "~/.tmux.conf reloaded"
