set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/pack/my-packages/start/Vundle.vim

call vundle#begin('~/.vim/pack/my-packages/start')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

"------------------------------------------
"--- Editing
"-----------------------------------------
Plugin 'christoomey/vim-sort-motion' " use gs
Plugin 'tomtom/tcomment_vim' " Commenting
Plugin 'tpope/vim-surround'
Plugin 'junegunn/vim-easy-align'
Plugin 'reedes/vim-pencil'

"------------------------------------------
"--- Linting / testing
"-----------------------------------------
" Plugin 'vim-syntastic/syntastic'
Plugin 'ngmy/vim-rubocop'
Plugin 'thoughtbot/vim-rspec'
Plugin 'w0rp/ale' " async linting

"------------------------------------------
"--- GUI changes
"-----------------------------------------
Plugin 'airblade/vim-gitgutter'
Plugin 'scrooloose/nerdtree.git'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'xuyuanp/nerdtree-git-plugin'
" Plugin 'jeffkreeftmeijer/vim-numbertoggle'

"------------------------------------------
"--- Languages
"-----------------------------------------
"
" Plugin 'jelera/vim-javascript-syntax' " doesnt seem to do anything?
Plugin 'chrisbra/Colorizer'
Plugin 'elixir-lang/vim-elixir'
Plugin 'godlygeek/tabular'
Plugin 'jparise/vim-graphql'
Plugin 'mxw/vim-jsx'
Plugin 'pangloss/vim-javascript'
Plugin 'plasticboy/vim-markdown'

"------------------------------------------
"--- Color Schemes
"-----------------------------------------
Plugin 'dracula/vim'
Plugin 'jacoborus/tender.vim'
Plugin 'morhetz/gruvbox'
Plugin 'obsidian'
Plugin 'tomasr/molokai'
Plugin 'adlawson/vim-sorcerer'

"------------------------------------------
"--- session handling
"-----------------------------------------
" Plugin 'tpope/vim-obsession'

"------------------------------------------
"--- Utilities
"-----------------------------------------
Plugin 'diepm/vim-rest-console'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'rking/ag.vim'
Plugin 'tpope/vim-rhubarb'
" Plugin 'ctrlpvim/ctrlp.vim' " replaced with fzf
" Plugin 'd11wtq/ctrlp_bdelete.vim' " goes with ctrlp

"------------------------------------------
"--- Other stuff
"-----------------------------------------
" Plugin 'christoomey/vim-tmux-navigator'
" Plugin 'craigemery/vim-autotag'
" Plugin 'godlygeek/tabular'
" Plugin 'kchmck/vim-coffee-script'
" Plugin 'lchi/vim-toffee'
" Plugin 'mustache/vim-mustache-handlebars'
" Plugin 'sjl/vitality.vim'
" Plugin 'slim-template/vim-slim'
" Plugin 'terryma/vim-multiple-cursors'
" Plugin 'tpope/vim-cucumber'
" Plugin 'tpope/vim-dispatch'
" Plugin 'tpope/vim-endwise'
" Plugin 'tpope/vim-surround'

call vundle#end()
"
"---------------------------------------------------------------"
"--- Editor
"---------------------------------------------------------------"
filetype plugin indent on
" set clipboard=unnamed
set mouse=a
set wrapmargin=0
" set cursorline " breaking shit!
set re=1
set relativenumber
set wildignore=*.keep,*~,*.swp
set incsearch
set hlsearch
set bs=indent,eol,start " Allow backspacing over everything in insert mode
set laststatus=2
:set dictionary="/usr/dict/words"
autocmd vimenter * set number
set tags=tags;
set nrformats-=octal
"---------------------------------------------------------------"
"--- Appearance
"---------------------------------------------------------------"
syntax enable
" colorscheme obsidian
color tender

let g:airline_powerline_fonts = 1
let g:airline_theme = 'tender'
" let g:airline#extensions#tabline#enabled = 1

let g:jsx_ext_required = 0
set statusline+=%#warningmsg#
set statusline+=%*
" set statusline+=%{SyntasticStatuslineFlag()} " no longer using syntastic

let g:NERDTreeWinSize=60 " nice big tree is it's easy to toggle off

"---------------------------------------------------------------"
"--- Linting
"---------------------------------------------------------------"
let g:ale_lint_on_save = 1
" let g:ale_lint_on_text_changed = 'normal'
let g:ale_linters = { 'javascript': ['eslint'],}
let g:ale_list_window_size = 3

"---------------------------------------------------------------"
"--- Indentation
"---------------------------------------------------------------"
" set tabstop=2 shiftwidth=2 expandtab
set expandtab
autocmd FileType * setlocal tabstop=2 shiftwidth=2
autocmd FileType javascript setlocal tabstop=4 shiftwidth=4
autocmd FileType javascript.jsx setlocal tabstop=4 shiftwidth=4
autocmd FileType elixir  setlocal tabstop=4 shiftwidth=4
autocmd FileType ruby  setlocal tabstop=2 shiftwidth=2
autocmd FileType yaml  setlocal tabstop=2 shiftwidth=2


"---------------------------------------------------------------"
"--- Mappings
"---------------------------------------------------------------"
" let mapleader = ","
let mapleader = " "
" escape key
ino jk <esc>
cno jk <C-c>

" remapping of ; to : for quick escape
" map ; :
" noremap ;; ;

nmap <silent> <C-k> :wincmd k<CR>
nmap <silent> <C-j> :wincmd j<CR>
nmap <silent> <C-h> :wincmd h<CR>
nmap <silent> <C-l> :wincmd l<CR>

nnoremap <silent> <Leader>+ :exe "vertical resize +10"<CR>
nnoremap <silent> <Leader>- :exe "vertical resize -10"<CR>

map <C-n> :NERDTreeToggle<CR>
map <Leader>n :NERDTreeFind<CR>

:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
nmap <Tab> :b#<CR>

 " formats single line string to json
map <Leader>j :Json<CR>

" fzf + ag commands
map <C-p> :Files<CR>
map <Leader>b :Buffers<CR>
map <Leader>g :GFiles<CR>

map \ :Ag<CR>
map <C-\> :Ag!<CR>

"find and replace
map <Leader>. :%s/


"delete without adding to clipboard
nnoremap <leader>d "_d
vnoremap <leader>d "_d

nnoremap <leader>D "_D
nnoremap <leader>dd "_dd

" paste and keep in clipboard
vnoremap <leader>p "_dP

nmap <leader>sw :StripTrailingWhitespaces<CR>

map <leader>t :TableFormat<CR>
map <leader>s :sort<CR>

map <leader>col :ColorToggle<CR>
map <leader>rn :set relativenumber!<CR>
map <Leader>cl :set cursorline!<CR>
map <Leader>w :set nowrap!<CR>
"---------------------------------------------------------------"
"--- Functions
"---------------------------------------------------------------"
:command! Json %!python -m json.tool

command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)


"Use TAB to complete when typing words, else inserts TABs as usual.
function! Tab_Or_Complete()
        if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~'^\w'
                return "\<C-N>"
        else
                return "\<Tab>"
        endif
endfunction

" via: http://rails-bestpractices.com/posts/60-remove-trailing-whitespace
" Strip trailing whitespace
function! <SID>StripTrailingWhitespaces()
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  %s/\s\+$//e
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction
command! StripTrailingWhitespaces call <SID>StripTrailingWhitespaces()

"---------------------------------------------------------------"
"--- NERdTREE stuff
"---------------------------------------------------------------"
" closes nerdtree if only open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" ignore files */
let NERDTreeIgnore = ['\.DAT$', '\.LOG1$', '\.LOG1$']
let NERDTreeIgnore += ['\.png$','\.jpg$','\.gif$','\.mp3$','\.flac$', '\.ogg$', '\.mp4$','\.avi$','.webm$','.mkv$','\.pdf$', '\.zip$', '\.tar.gz$', '\.rar$']

"---------------------------------------------------------------"
"--- Typing stuff
"---------------------------------------------------------------"
if has("spell")
  " turn spelling on by default
  set spell

  " toggle spelling with F4 key
  map <F4> :set spell!<CR><Bar>:echo "Spell Check: " . strpart("OffOn", 3 * &spell, 3)<CR>

  " they were using white on white
  highlight PmenuSel ctermfg=black ctermbg=lightgray

  " limit it to just the top 10 items
  set sps=best,10                    
endif

"---------------------------------------------------------------" */
"--- Retired */
"---------------------------------------------------------------" */
" let g:ctrlp_custom_ignore = { */
"                         \ 'dir': '\v[\/](\.git|tmp|node_modules|app_build|build)' */
"                         \} */
" let g:ctrlp_show_hidden = 1 */
" call ctrlp_bdelete#init() */
