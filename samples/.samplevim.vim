compatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
let g:vundle_default_git_proto = 'git'
call vundle#begin()

Plugin 'airblade/vim-gitgutter'
Plugin 'bling/vim-airline'
Plugin 'craigemery/vim-autotag'
Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'rking/ag.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'tomasr/molokai'
Plugin 'altercation/vim-colors-solarized'
Plugin 'reedes/vim-colors-pencil'
Plugin 'tpope/vim-cucumber'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'godlygeek/tabular'
Plugin 'kchmck/vim-coffee-script'
Plugin 'scrooloose/syntastic'
Plugin 'sjl/vitality.vim'
Plugin 'lchi/vim-toffee'
Plugin 'slim-template/vim-slim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-dispatch'
Plugin 'tomtom/tcomment_vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ngmy/vim-rubocop'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'thoughtbot/vim-rspec'
Plugin 'obsidian'
" Plugin 'stephenmckinney/vim-solarized-powerline'

call vundle#end()

filetype plugin indent on
syntax on

" setting hidden allows undo to work after buffer was closed
set hidden
set noswapfile
set nobackup
set mouse=a
set nocompatible
set tabstop=2 shiftwidth=2 expandtab
set number
set nowrap
set backspace=indent,eol,start
set textwidth=0
set wrapmargin=0
set scrolloff=4
set t_Co=256
set incsearch

set clipboard=unnamed
map ,n :NERDTreeFind<CR>
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = {
      \ 'dir': '\v[\/](\.git|tmp|node_modules)'
      \ }
set cursorline
set wildignore=*.keep,*~,*.swp
" To quit all files quickly - useful for quitting 'git d' by holding down on Q
map Q :qa<CR>
au FileType css setl ofu=csscomplete#CompleteCSS
au FocusLost * :wa

" Toggle between the last 2 files
nmap <Tab> :b#<CR>

"Use TAB to complete when typing words, else inserts TABs as usual.
function! Tab_Or_Complete()
  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
    return "\<C-N>"
  else
    return "\<Tab>"
  endif
endfunction
:inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
:set dictionary="/usr/dict/words"

" Airline settings
let g:airline_powerline_fonts = 1
set laststatus=2

" Function and key mapping for running cucumber test
" ,t - Run scenario under cursor
" ,T - Run whole feature file
let mapleader = ","
autocmd FileType cucumber nmap <leader>t :call RunCucumberTest(line('.'))<CR>
autocmd FileType cucumber nmap <leader>T :call RunCucumberTest()<CR>
function! RunCucumberTest(...)
  let cmd = 'bundle exec cucumber ' . expand('%') . (a:0 == 1 ? ':'.line('.') : '')
  if strlen(cmd > 0)
    execute ':wa'
    execute ':Dispatch ' . cmd
  elseif
    echoerr "No test command to run"
  endif
endfunction

if &term =~ '^screen'
  " tmux knows the extended mouse mode
  set ttymouse=xterm2
endif

" Auto remove all trailing characters
autocmd BufWritePre * :%s/\s\+$//e

set timeout         " Do time out on mappings and others
set timeoutlen=2000 " Wait {num} ms before timing out a mapping

" When you’re pressing Escape to leave insert mode in the terminal, it will by
" default take a second or another keystroke to leave insert mode completely
" and update the statusline. This fixes that. I got this from:
" https://powerline.readthedocs.org/en/latest/tipstricks.html#vim
if !has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
  augroup END
endif

" Format cucumber table
map \| :Tab /\|<CR>

map <F2> :set background=light<CR>
map <F3> :set background=dark<CR>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

map VIM 0 ^

" Quick togo last buffer basically
nnoremap <leader><leader> <c-^>

" Create window splits easier. The default
" way is Ctrl-w,v and Ctrl-w,s. I remap
" this to vv and ss
nnoremap <silent> vv <C-w>v
nnoremap <silent> ss <C-w>s

nmap <silent> ,qc :cclose<CR>
nmap <silent> ,qo :copen<CR>

" NERDTREE
nnoremap <C-f> :NERDTreeFind<cr><cr>
noremap <C-n> :NERDTreeToggle<CR>

" List Buffers
nmap <silent> <Leader>b :CtrlPBuffer<CR>

nmap <leader>w :w!<cr>
nmap <leader>q :q<CR>

" hit ,f to find the definition of the current class
" this uses ctags. the standard way to get this is Ctrl-]
nnoremap <silent> ,f <C-]>

" Index ctags from any project, including those outside Rails
map <Leader>ct :!ctags -R .<CR>

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
nmap <leader>sw :StripTrailingWhitespaces<CR>

if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  " Map cursor for insert mode
  let &t_SI .= "\<Esc>[5 q"
  " solid block
  let &t_EI .= "\<Esc>[2 q"
  " 1 or 0 -> blinking block
  " 3 -> blinking underscore
  " Recent versions of xterm (282 or above) also support
  " 5 -> blinking vertical bar
  " 6 -> solid vertical bar
endif

" Local config
if filereadable($HOME . "/.vimrc.local")
  source ~/.vimrc.local
endif

let g:vimrubocop_keymap = 0
nmap <Leader>r :RuboCop<CR>

abbr pry! require 'pry'; binding.pry
abbr saop! save_and_open_page

" Autosave
" let g:auto_save = 1
" let g:auto_save_in_insert_mode = 0
"
set incsearch
set hlsearch

highlight GitGutterAdd ctermfg=green
highlight GitGutterChange ctermfg=yellow
highlight GitGutterDelete ctermfg=red
highlight GitGutterChangeDelete ctermfg=yellow
" colorscheme pencil
" colorscheme solarized
colorscheme obsidian
" set background=light

" let g:airline_theme='pencil'
let g:gitgutter_sign_column_always = 1
let g:gitgutter_highlight_lines = 0

" highlight clear SignColumn
highlight GitGutterAdd guibg=green
highlight GitGutterChange guibg=yellow
highlight GitGutterDelete guibg=red
highlight GitGutterChangeDelete guibg=yellow

" Tab completion
" will insert tab at beginning of line,
" will use completion if not at beginning
set wildmode=list:longest,list:full
function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction
inoremap <Tab> <c-r>=InsertTabWrapper()<cr>
inoremap <S-Tab> <c-n>

" RSpec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

" let g:multi_cursor_next_key='<C-m>'
" let g:multi_cursor_prev_key='<C-p>'
" let g:multi_cursor_skip_key='<C-x>'
" let g:multi_cursor_quit_key='<Esc>'

set tags+=gems.tags

"" Local config
if filereadable($HOME . "/.vimrc.local")
  source ~/.vimrc.local
endif
set scrolloff=72

