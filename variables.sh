# source: https://github.com/samoshkin/dotfiles

# =============
#    EXPORT
# =============

export ZSH="$HOME/.oh-my-zsh"
export EDITOR='vim'
export PATH="$HOME/bin:$PATH"
export GOPATH=$HOME/go
export ES_HOME=/usr/local/bin/elasticsearch
export RUBYMOTION_ANDROID_SDK=/Users/iacutone/.rubymotion-android/sdk
export RUBYMOTION_ANDROID_NDK=/Users/iacutone/.rubymotion-android/ndk

# Configure fzf, command line fuzzyf finder
FD_OPTIONS="--hidden --follow --exclude .git --exclude node_modules"
export FZF_DEFAULT_OPTS="--no-mouse --height 50% -1 --reverse --multi --inline-info --preview='[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2> /dev/null | head -300' --preview-window='right:hidden:wrap' --bind='f3:execute(bat --style=numbers {} || less -f {}),f2:toggle-preview,ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-a:select-all+accept,ctrl-y:execute-silent(echo {+} | pbcopy),ctrl-x:execute(rm -i {+})+abort'"
# Use git-ls-files inside git repo, otherwise fd
export FZF_DEFAULT_COMMAND="git ls-files --cached --others --exclude-standard || fd --type f --type l $FD_OPTIONS"
export FZF_CTRL_T_COMMAND="fd $FD_OPTIONS"
export FZF_ALT_C_COMMAND="fd --type d $FD_OPTIONS"

export BAT_PAGER="less -R"
export BAT_THEME="Monokai Extended"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
