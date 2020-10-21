set -o vi

# =============
#    ALIAS
# =============

alias tml="tmux list-sessions"
alias tma="tmux -2 attach -t $1"
alias tmk="tmux kill-session -t $1"
alias mux="tmuxinator $1"
alias subl="sublime"
alias vi='nvim'

ZSH_THEME="theunraveler"
plugins=(git bundler osx rake ruby rails history-substring-search)


# =============
#    SOURCE
# =============

source "$HOME/dotfiles/variables.sh"
source $ZSH/oh-my-zsh.sh
source ~/.bash_profile
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh
export GOPATH=$HOME/go

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

function configureviewapp {
  pr_id=$1

  heroku config:set WEBSOLR_URL=$ARTFULLY_WEBSOLR_URL --app artfully-staging-pr-$pr_id && heroku config:set DATABASE_URL=$ARTFULLY_DATABASE_URL --app artfully-staging-pr-$pr_id
  heroku run rake db:migrate --app artfully-staging-pr-$pr_id
#  heroku config:set SNS_TOPIC_NEW_DONATION=$ARTFULLY_SNS_TOPIC_NEW_DONATION --app artfully-staging-pr-$pr_id
#  heroku config:set SNS_TOPIC_NEW_FS_DONATION=$ARTFULLY_SNS_TOPIC_NEW_FS_DONATION --app artfully-staging-pr-$pr_id

  echo "======================"
  echo "https://artfully-staging-pr-$pr_id.herokuapp.com/admin configured."
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

# ===================
#    PLUGINS
# ===================

# brew install zsh-autosuggestions
source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# brew install zsh-syntax-highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# z.sh
. ~/dotfiles/z.sh

eval "$(direnv hook zsh)"

# asdf
. $(brew --prefix asdf)/asdf.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

db_set () {
    echo "$1,$2" >> database
}

db_get () {
    grep "^$1," database | sed -e "s/^$1,//" | tail -n 1
}
