set -o vi
export EDITOR=vim
export PATH="/usr/local/sbin:$PATH"

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
  macos
  zsh-autosuggestions
  zsh-syntax-highlighting
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

  youtube-dl -f 'bestaudio[ext=m4a]/mp4' "$*" \
    --output="~/Downloads/audio/%(title)s.%(ext)s" \
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

# z.sh
. ~/dotfiles/z.sh

# asdf
. /opt/homebrew/opt/asdf/libexec/asdf.sh

db_set () {
    echo "$1,$2" >> database
}

db_get () {
    grep "^$1," database | sed -e "s/^$1,//" | tail -n 1
}

<<<<<<< Updated upstream
. /opt/homebrew/opt/asdf/libexec/asdf.sh
=======

### CARS VPN
export VPN_HOST=tqavc02-vpn.cars.com/JC
# VPN_USER is YOUR_CARS_USERNAME_WITHOUT_@cars.com
export VPN_USER=eiacutone

function vpninfo() {
    if [[ -z $VPN_HOST ]]; then
        echo "Please set VPN_HOST env var"
        return
    fi
    if [[ -z $VPN_USER ]]; then
        echo "Please set VPN_USER env var"
        return
    fi
    echo "Starting the vpn ..."
    echo "Connecting for USER: $VPN_USER"
}

# start the VPN with VPN-slice. As of this writing, these slices work for many
# AWS-based services as well as the ODS DB (while that's still alive).
# Things change, so you may end up needing to add some new CIDR blocks to the slice command.
# You may say to yourself I understand CIDR notation, why didn't the author just
# use 172.0.0.0/8 or 172.0.0.0/12, which should capture all the individual IP blocks.
# Reader, the author did try that very thing, and even verified with other colleagues
# that 172.0.0.0/8 does not work as we wish.
# The author humbly invites you to try to come up with a cleverer solution.

function vpnup() {
    vpninfo
    sudo openconnect $VPN_HOST \
        --user=$VPN_USER \
        --protocol=anyconnect \
        --background \
        -s 'vpn-slice 172.16.0.0/12 ods_db=ods-prod-scan.cars.com=172.20.67.144 ods_cpx=cpx3-scan.cars.com=172.20.67.145 -K'
}

# start the VPN without VPN-slice (i.e. route all traffic through the VPN)
# useful for debugging to figure out where a service may be located.

function vpnall() {
    vpninfo
    sudo openconnect $VPN_HOST \
        --user=$VPN_USER \
        --protocol=anyconnect \
        --background
}

# shutdown / kill the VPN

function vpndown() {
    sudo kill -2 `pgrep openconnect`
    sudo kill -9 `pgrep vpn-slice`
}
