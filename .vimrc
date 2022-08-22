filetype plugin indent on " Needs to go before autocmds
syntax enable " Needs to go before autocmds

set backspace=indent,eol,start              " Allow backspacing over everything in insert mode
set clipboard=unnamed                       " just too annoying without this
set completeopt=noinsert,menuone,noselect   " note that must keep noinsert in completeopt, the others is optional
set grepprg=rg\ --hidden\ --glob\ '!.git'\ --vimgrep\ --with-filename
set autowrite               " write if modified, such as when running :make
set expandtab               " tabs are spaces
set fillchars=vert:│,fold:· " char between panels
set hidden                  " allows hiding modified buffers
set hlsearch                " highlight searches
set incsearch               " search as characters are entered
set lazyredraw              " redraw only when we need to.
set nomodeline              " https://github.com/numirias/security/blob/master/doc/2019-06-04_ace-vim-neovim.md
set regexpengine=1          " TODO really slow without this??
set shiftwidth=2            " number of spaces in tab when editing
set showtabline=2           " Show tabline
set smartcase               " search ignores case unless capitals present
set sps=best,10             " spell only shows top 10 results
set tabstop=2               " number of visual spaces per TAB
set wildmenu                " visual autocomplete for command menu
set equalalways             " window size changes automatically

set dictionary="/usr/dict/words"
set foldnestmax=3
set ignorecase
set laststatus=2
set mouse=a
set noshowmode
set nrformats-=octal
set omnifunc=LanguageClient#complete
set path+=**
set scrolloff=3
set shortmess+=c
set signcolumn=yes
set splitbelow
set splitright
set tags=.tags;
set wildignore=*.keep,*~,*.swp
set wrapmargin=0
set number
set relativenumber

colorscheme slate

let mapleader = " "
let maplocalleader = ","
ino jk <esc>
cno jk <C-c>
" move vertically by visual line
" nnoremap j gj
" nnoremap k gk

" MAPS ON COMMANDS I DONT LIKE
" map <C-B>
" map <C-G>
" map <C-Q>
" map <C-Y>
map <C-F> :%s/
nmap <silent> <C-H> :wincmd h<CR>
nmap <silent> <C-J> :wincmd j<CR>
nmap <silent> <C-K> :wincmd k<CR>
nmap <silent> <C-L> :wincmd l<CR>

imap <C-@> <C-Space>

" Debugging  {{{
" set verbose=9
" set verbosefile=~/vim_debug.txt
" }}}
