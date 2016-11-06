# Path to your oh-my-zsh installation.
export ZSH=/Users/iacutone/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="theunraveler"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git bundler osx rake ruby rails)

# User configuration
export PATH=/usr/local/sbin:/usr/local/bin:${PATH}
export PATH="$HOME/bin:$PATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias subl="sublime"
alias vi='vim'

export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

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

  heroku addons:destroy heroku-postgresql:hobby-dev --app artfully-staging-pr-$pr_id --confirm artfully-staging-pr-$pr_id && heroku config:set WEBSOLR_URL=$ARTFULLY_WEBSOLR_URL --app artfully-staging-pr-$pr_id && heroku config:set DATABASE_URL=$ARTFULLY_DATABASE_URL --app artfully-staging-pr-$pr_id
  heroku run rake db:migrate --app artfully-staging-pr-$pr_id
  heroku config:set SNS_TOPIC_NEW_DONATION=$ARTFULLY_SNS_TOPIC_NEW_DONATION --app artfully-staging-pr-$pr_id
  heroku config:set SNS_TOPIC_NEW_FS_DONATION=$ARTFULLY_SNS_TOPIC_NEW_FS_DONATION --app artfully-staging-pr-$pr_id

  echo "======================"
  echo "https://artfully-staging-pr-$pr_id.herokuapp.com/admin configured."
}
