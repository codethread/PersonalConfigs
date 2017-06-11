set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/pack/my-packages/start/Vundle.vim

call vundle#begin('~/.vim/pack/my-packages/start')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Editing
Plugin 'tomtom/tcomment_vim' " Commenting

" File Utils
Plugin 'rking/ag.vim' " Used for searching like ack
Plugin 'kien/ctrlp.vim'

" GUI changes
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree.git'

"Plugin 'mxw/vim-jsx'
" Plugin 'reedes/vim-colors-pencil'
" Plugin 'tpope/vim-cucumber'
" Plugin 'tpope/vim-endwise'
" Plugin 'tpope/vim-surround'
" Plugin 'kchmck/vim-coffee-script'
" Plugin 'craigemery/vim-autotag'
" Plugin 'thoughtbot/vim-rspec'
" Plugin 'terryma/vim-multiple-cursors'
" Plugin 'ngmy/vim-rubocop'
" Plugin 'tpope/vim-dispatch'
" Plugin 'slim-template/vim-slim'
" Plugin 'christoomey/vim-tmux-navigator'
" Plugin 'mustache/vim-mustache-handlebars'
" Plugin 'lchi/vim-toffee'

" Other stuff
" Plugin 'godlygeek/tabular'
" Plugin 'scrooloose/syntastic'
" Plugin 'sjl/vitality.vim'

" Languages
Plugin 'elixir-lang/vim-elixir'

" Color Schemes
Plugin 'obsidian'
Plugin 'jacoborus/tender.vim'
Plugin 'tomasr/molokai'
Plugin 'altercation/vim-colors-solarized'

call vundle#end()

syntax on
filetype plugin indent on

syntax enable

"""""""""""""""""""""""""""""""""""""""""
"""""""""""" THEMES
"""""""""""""""""""""""""""""""""""""""""
" set background=dark
" colorscheme solarized

colorscheme tender
let g:airline_powerline_fonts = 1
let g:airline_theme = 'tender'



"""""""""""""""""""""""""""""""""""""""""
"""""""""""" MAPPINGS
"""""""""""""""""""""""""""""""""""""""""

" escape key
ino jk <esc>
cno jk <C-c>

" remapping of ; to : for quick escape
map ; :
noremap ;; ;

map <C-n> :NERDTreeToggle<CR>
map ,n :NERDTreeFind<CR>
autocmd vimenter * set number
"Use TAB to complete when typing words, else inserts TABs as usual.
function! Tab_Or_Complete()
	if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~'^\w'
		return "\<C-N>"
	else
		return "\<Tab>"
	endif
endfunction

:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
:set dictionary="/usr/dict/words"
let g:ctrlp_show_hidden = 1

nmap <Tab> :b#<CR>
let g:ctrlp_custom_ignore = {
			\ 'dir': '\v[\/](\.git|tmp|node_modules)'
\}
