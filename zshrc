set -o vi
export EDITOR=vi

# =============
#    ALIAS
# =============

alias tml="tmux list-sessions"
alias tma="tmux -2 attach -t $1"
alias tmk="tmux kill-session -t $1"
alias mux="tmuxinator $1"
alias subl="sublime"
alias vi='nvim'

ZSH_THEME=theunraveler
plugins=(git
  bundler 
  history-substring-search
  zsh-autosuggestions
  zsh-syntax-highlighting
  osx
)


# =============
#    SOURCE
# =============

source "$HOME/Dropbox/variables.sh"
source $ZSH/oh-my-zsh.sh

# =============
#   FUNCTIONS
# =============

function play {
  # Skip DASH manifest for speed purposes. This might actually disable
  # being able to specify things like 'bestaudio' as the requested format,
  # but try anyway.
  # Get the best audio that isn't WebM, because afplay doesn't support it.
  # Use "$*" so that quoting the requested song isn't necessary.
  youtube-dl --default-search=ytsearch: \
             --youtube-skip-dash-manifest \
             --output="${TMPDIR:-/tmp/}%(title)s-%(id)s.%(ext)s" \
             --restrict-filenames \
             --format="bestaudio[ext!=webm]" \
             --exec=afplay "$*"
}

function mp3 {
  # Download all of the things to /Downloads/audio/

  youtube-dl --default-search=ytsearch: \
             --restrict-filenames \
             --format=bestaudio \
             --audio-format=mp3 \
             --audio-quality=1 "$*" \
             --output="~/Downloads/audio/%(title)s.%(ext)s"
}


# =============
#      FZF
# =============

# Returns whether the given command is executable or aliased.
_has() {
  return $( whence $1 >/dev/null )
}

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
if [ -e /usr/local/opt/fzf/shell/completion.zsh ]; then
  source /usr/local/opt/fzf/shell/key-bindings.zsh
  source /usr/local/opt/fzf/shell/completion.zsh
fi

# z.sh
. ~/dotfiles/z.sh

# asdf
. $(brew --prefix asdf)/asdf.sh
. $(brew --prefix asdf)/etc/bash_completion.d/asdf.bash

db_set () {
    echo "$1,$2" >> database
}

db_get () {
    grep "^$1," database | sed -e "s/^$1,//" | tail -n 1
}
