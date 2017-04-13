#
set -g status-right-length 60
set -g status-right "#[fg=blue]#S #I:#P #[fg=yellow]:: %d %b %Y "

# use UTF8
set -g utf8
set-window-option -g utf8 on

# make tmux display things in 256 colors
set -g default-terminal "screen-256color"

# default statusbar colors
set-option -g status-bg colour235

