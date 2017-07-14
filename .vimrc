set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/pack/my-packages/start/Vundle.vim

call vundle#begin('~/.vim/pack/my-packages/start')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Editing
Plugin 'tomtom/tcomment_vim' " Commenting

" File navigation
Plugin 'rking/ag.vim' " Used for searching like ack
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'd11wtq/ctrlp_bdelete.vim'

Plugin 'junegunn/fzf.vim'

" Linting
" Plugin 'vim-syntastic/syntastic'
Plugin 'w0rp/ale' " async linting 

" GUI changes
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree.git'

" Languages
Plugin 'elixir-lang/vim-elixir'
Plugin 'pangloss/vim-javascript'
" Plugin 'jelera/vim-javascript-syntax' " doesnt seem to do anything?
Plugin 'mxw/vim-jsx'

" Color Schemes
Plugin 'obsidian'
Plugin 'jacoborus/tender.vim'
Plugin 'tomasr/molokai'
Plugin 'reedes/vim-colors-pencil'

" session handling
Plugin 'tpope/vim-obsession'

" Other stuff
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
" Plugin 'godlygeek/tabular'
" Plugin 'sjl/vitality.vim'


call vundle#end()

syntax on
filetype plugin indent on

syntax enable
let g:jsx_ext_required = 0
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

set laststatus=2
" let g:airline#extensions#tabline#enabled = 1

" let g:ale_lint_on_save = 1
" let g:ale_lint_on_text_changed = 'normal'
let g:ale_linters = { 'javascript': ['eslint'],}
let g:ale_list_window_size = 3
" " set tabstop=2 shiftwidth=2 expandtab
set expandtab
autocmd FileType * setlocal tabstop=4 shiftwidth=4
autocmd FileType javascript setlocal tabstop=4 shiftwidth=4
autocmd FileType javascript.jsx setlocal tabstop=4 shiftwidth=4
autocmd FileType elixir  setlocal tabstop=4 shiftwidth=4

set clipboard=unnamed
set nowrap
set wrapmargin=0
" set cursorline " breaking shit!
set wildignore=*.keep,*~,*.swp
" set incsearch
set hlsearch
""""""""""""""""""""""""""""""""""""""""
"""""""""""" THEMES
"""""""""""""""""""""""""""""""""""""""""
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

let mapleader = ","

:command! Json %!python -m json.tool
map <Leader>j :Json<CR>

let g:ctrlp_show_hidden = 1
call ctrlp_bdelete#init()

nmap <Tab> :b#<CR>
let g:ctrlp_custom_ignore = {
                        \ 'dir': '\v[\/](\.git|tmp|node_modules|app_build|build)'
                        \}
"delete without adding to clipboard
nnoremap <leader>d "_d 
vnoremap <leader>d "_d

nnoremap <leader>D "_D 
nnoremap <leader>dd "_dd 

" paste and keep in clipboard
vnoremap <leader>p "_dP

nmap <silent> <C-k> :wincmd k<CR>
nmap <silent> <C-j> :wincmd j<CR>
nmap <silent> <C-h> :wincmd h<CR>
nmap <silent> <C-l> :wincmd l<CR>
