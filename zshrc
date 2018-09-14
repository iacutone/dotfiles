# =============
#    ALIAS
# =============

alias tml="tmux list-sessions"
alias tma="tmux -2 attach -t $1"
alias tmk="tmux kill-session -t $1"
alias mux="tmuxinator $1"
alias subl="sublime"
alias vi='vim'


# =============
#    EXPORT
# =============

export ZSH=/Users/iacutone/.oh-my-zsh
export EDITOR='vim'
export PATH="$HOME/bin:$PATH"
export GOPATH=$HOME/go

ZSH_THEME="theunraveler"
plugins=(git bundler osx rake ruby rails history-substring-search)


# =============
#    SOURCE
# =============

source $ZSH/oh-my-zsh.sh
source ~/.bash_profile
source $HOME/.rvm/scripts/rvm


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
 # heroku config:set SNS_TOPIC_NEW_FS_DONATION=$ARTFULLY_SNS_TOPIC_NEW_FS_DONATION --app artfully-staging-pr-$pr_id

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

# fzf + ag configuration
if _has fzf && _has ag; then
  export FZF_DEFAULT_COMMAND='ag --nocolor -g ""'
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_DEFAULT_OPTS='
  --color fg:242,bg:236,hl:65,fg+:15,bg+:239,hl+:108
  --color info:108,prompt:109,spinner:108,pointer:168,marker:168
  '
fi


# ===================
#    PLUGINS
# ===================

# brew install zsh-autosuggestions
source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# brew install zsh-syntax-highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# inspired by https://github.com/fatih/dotfiles/blob/master/zshrc
