set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-rake'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-endwise'
Plugin 'janko-m/vim-test'
Plugin 'majutsushi/tagbar'
Plugin 'airblade/vim-gitgutter'
Plugin 'elixir-lang/vim-elixir'
Plugin 'kchmck/vim-coffee-script'
Plugin 'mattn/emmet-vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'Valloric/YouCompleteMe'
Plugin 'Raimondi/delimitMate'
Plugin 'rizzatti/dash.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'mileszs/ack.vim'
Plugin 'stefanoverna/vim-i18n'
Plugin 'pbrisbin/vim-mkdir'
Plugin 'altercation/vim-colors-solarized'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'easymotion/vim-easymotion'
Plugin 'rhysd/devdocs.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'rstacruz/sparkup'
Plugin 'farseer90718/vim-taskwarrior'

call vundle#end()

set shell=zsh
set runtimepath^=~/.vim/bundle/ctrlp.vim
autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,css,scss,cucumber,coffee set ai sw=2 sts=2 et
let mapleader = ','
syntax enable
set background=dark
let g:solarized_termcolors=256
colorscheme solarized
set backspace=indent,eol,start
set encoding=utf-8


let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

nmap <silent> <leader>s :TestNearest<CR>
nmap <silent> <leader>t :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>
" let test#strategy = "dispatch"
let test#filename_modifier = ':.'

let g:tagbar_ctags_bin='/usr/local/bin/ctags'
let g:tagbar_width=30
let g:ctrlp_max_files=0
let g:ctrlp_max_depth=40
let g:ctrlp_show_hidden = 1
noremap <silent> <Leader>y :TagbarToggle
nmap <silent> <leader>d <Plug>DashSearch
let b:surround_{char2nr('=')} = "<%= \r %>"
let b:surround_{char2nr('-')} = "<% \r %>"
let g:airline_theme='jellybeans'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
vmap <Leader>z :call I18nTranslateString()<CR>
vmap <Leader>dt :call I18nDisplayTranslation()<CR>

set laststatus=2
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:task_rc_override = 'rc.defaultwidth=0'
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode
