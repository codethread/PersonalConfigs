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
Plugin 'tomtom/tcomment_vim' " Commenting

"------------------------------------------
"--- Linting / testing
"-----------------------------------------
" Plugin 'vim-syntastic/syntastic'
Plugin 'w0rp/ale' " async linting 
Plugin 'ngmy/vim-rubocop'
Plugin 'thoughtbot/vim-rspec'

"------------------------------------------
"--- GUI changes
"-----------------------------------------
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdtree.git'
Plugin 'Xuyuanp/nerdtree-git-plugin'
" Plugin 'jeffkreeftmeijer/vim-numbertoggle'

"------------------------------------------
"--- Languages
"-----------------------------------------
"
Plugin 'elixir-lang/vim-elixir'
Plugin 'pangloss/vim-javascript'
" Plugin 'jelera/vim-javascript-syntax' " doesnt seem to do anything?
Plugin 'mxw/vim-jsx'
Plugin 'jparise/vim-graphql'

"------------------------------------------
"--- Color Schemes
"-----------------------------------------
Plugin 'obsidian'
Plugin 'jacoborus/tender.vim'
Plugin 'tomasr/molokai'
Plugin 'reedes/vim-colors-pencil'

"------------------------------------------
"--- session handling
"-----------------------------------------
" Plugin 'tpope/vim-obsession'

"------------------------------------------
"--- Utilities
"-----------------------------------------
Plugin 'rking/ag.vim'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'diepm/vim-rest-console'
Plugin 'tpope/vim-rhubarb'
" Plugin 'ctrlpvim/ctrlp.vim' " replaced with fzf
" Plugin 'd11wtq/ctrlp_bdelete.vim' " goes with ctrlp

"------------------------------------------
"--- Other stuff
"-----------------------------------------
" Plugin 'tpope/vim-cucumber'
" Plugin 'tpope/vim-endwise'
" Plugin 'tpope/vim-surround'
" Plugin 'kchmck/vim-coffee-script'
" Plugin 'craigemery/vim-autotag'
" Plugin 'terryma/vim-multiple-cursors'
" Plugin 'tpope/vim-dispatch'
" Plugin 'slim-template/vim-slim'
" Plugin 'christoomey/vim-tmux-navigator'
" Plugin 'mustache/vim-mustache-handlebars'
" Plugin 'lchi/vim-toffee'
" Plugin 'godlygeek/tabular'
" Plugin 'sjl/vitality.vim'

call vundle#end()

"---------------------------------------------------------------"
"--- Editor
"---------------------------------------------------------------"
filetype plugin indent on
" set clipboard=unnamed
set mouse=a
set nowrap!
set wrapmargin=0
" set cursorline " breaking shit!
map <Leader>c :set cursorline!<CR>
set wildignore=*.keep,*~,*.swp
" set incsearch
set hlsearch
set bs=indent,eol,start " Allow backspacing over everything in insert mode
set laststatus=2
:set dictionary="/usr/dict/words"
autocmd vimenter * set number

"---------------------------------------------------------------"
"--- Appearance
"---------------------------------------------------------------"
syntax enable
colorscheme tender

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
" let g:ale_lint_on_save = 1
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
map ; :
noremap ;; ;

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

"---------------------------------------------------------------"
"--- NERdTREE stuff
"---------------------------------------------------------------"
" highlights
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='.  a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'.  a:extension .'$#'
endfunction

  " call NERDTreeHighlightFile('', 'yellow', 'none', 'yellow', '#151515')
  " call NERDTreeHighlightFile('', 'green', 'none', 'green', '#151515')
  " call NERDTreeHighlightFile('', 'cyan', 'none', 'cyan', '#151515')
  " call NERDTreeHighlightFile('', 'blue', 'none', '#3366FF', '#151515')
  " call NERDTreeHighlightFile('', 'Magenta', 'none', '#ff00ff', '#151515')
  " call NERDTreeHighlightFile('', 'Red', 'none', 'red', '#151515')

  " call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
  " call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
  " call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
  "
  " call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
  " call NERDTreeHighlightFile('sh', 'yellow', 'none', 'yellow', '#151515')
  " call NERDTreeHighlightFile('md', 'Magenta', 'none', '#ff00ff', '#151515')
  " call NERDTreeHighlightFile('yml', 'Magenta', 'none', '#ff00ff', '#151515')
  " call NERDTreeHighlightFile('config','Magenta', 'none', '#ff00ff', '#151515')
  " call NERDTreeHighlightFile('conf', 'Magenta', 'none', '#ff00ff', '#151515') 
  " call NERDTreeHighlightFile('zsh', 'Magenta', 'none', '#ff00ff', '#151515') 
  "
  " call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
  " call NERDTreeHighlightFile('css', 'green', 'none', 'green', '#151515')
  " call NERDTreeHighlightFile('scss', 'Magenta', 'none', '#ff00ff', '#151515')
  " call NERDTreeHighlightFile('sass', 'Magenta', 'none', '#ff00ff', '#151515')
  "
  " call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')

  " call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
  " call NERDTreeHighlightFile('jsx', 'cyan', 'none', 'cyan', '#151515') " ott

" closes nerdtree if only open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" ignore files
let NERDTreeIgnore = ['\.DAT$', '\.LOG1$', '\.LOG1$']
let NERDTreeIgnore += ['\.png$','\.jpg$','\.gif$','\.mp3$','\.flac$', '\.ogg$', '\.mp4$','\.avi$','.webm$','.mkv$','\.pdf$', '\.zip$', '\.tar.gz$', '\.rar$']
"---------------------------------------------------------------"
"--- Retired
"---------------------------------------------------------------"
" let g:ctrlp_custom_ignore = {
"                         \ 'dir': '\v[\/](\.git|tmp|node_modules|app_build|build)'
"                         \}
" let g:ctrlp_show_hidden = 1
" call ctrlp_bdelete#init()
