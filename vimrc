set shell=/bin/zsh

set nocompatible
filetype off
set runtimepath^=~/.vim/bundle/ctrlp.vim
execute pathogen#infect()
autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et
set grepprg=ack
let mapleader = ','
syntax enable
set background=dark
let g:solarized_termcolors=256
colorscheme solarized

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-fugitive'
Plugin 'thoughtbot/vim-rspec'
Plugin 'majutsushi/tagbar'
Plugin 'airblade/vim-gitgutter'
Plugin 'elixir-lang/vim-elixir'
Plugin 'kchmck/vim-coffee-script'

call vundle#end()
filetype plugin indent on

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>
let g:rspec_command="! bin/rspec {spec}"
let g:tagbar_ctags_bin='/usr/local/bin/ctags'
let g:tagbar_width=30
noremap <silent> <Leader>y :TagbarToggle

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" STATUS LINE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2
if has('statusline')
  set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)
  set statusline+=%{fugitive#statusline()}
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PROMOTE VARIABLE TO RSPEC LET
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! PromoteToLet()
  :normal! dd
  " :exec '?^\s*it\>'
  :normal! P
  :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :normal ==
endfunction
:command! PromoteToLet :call PromoteToLet()
:map <leader>L :PromoteToLet<cr>
