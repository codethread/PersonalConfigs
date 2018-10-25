call plug#begin('~/.vim/plugged')
"------------------------------------------
"--- Editing
"-----------------------------------------
Plug 'tpope/vim-sensible'
" Plug 'christoomey/vim-sort-motion' " use gs
Plug 'tomtom/tcomment_vim' " Commenting
" Plug 'tpope/vim-surround'
" Plug 'junegunn/vim-easy-align'
" Plug 'reedes/vim-pencil'
" Plug 'tpope/vim-repeat'
" Plug 'kshenoy/vim-signature'
" Plug 'wikitopian/hardmode'
" Plug 'ddrscott/vim-window'
" Plug 'danro/rename.vim'
" Plug 'plasticboy/vim-markdown'
" Plug 'kannokanno/previm'
" Plug 'godlygeek/tabular'
" Plug 'justinmk/vim-sneak'
" Plug 'tmhedberg/matchit'

"------------------------------------------
"--- Linting / testing
"-----------------------------------------
" Plug 'ngmy/vim-rubocop'
" Plug 'thoughtbot/vim-rspec'
" Plug 'w0rp/ale' " async linting

" "------------------------------------------
" "--- GUI changes
" "-----------------------------------------
" Plug 'airblade/vim-gitgutter'
" Plug 'Shougo/denite.nvim'
" Plug 'tpope/vim-fugitive'
" Plug 'tpope/vim-rhubarb'
" Plug 'vim-airline/vim-airline' " TODO slows startup replace
" Plug 'vim-airline/vim-airline-themes'
" Plug 'gcmt/taboo.vim'
" Plug 'majutsushi/tagbar'
" Plug 'hecal3/vim-leader-guide'
" Plug 'Yggdroot/indentLine'
" Plug 'junegunn/goyo.vim'
" Plug 'connorholyday/vim-snazzy'

" "------------------------------------------
" "--- Languages
" "-----------------------------------------
" Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
" Plug 'moll/vim-node'
" Plug 'sheerun/vim-polyglot'
" Plug 'chrisbra/Colorizer'
" Plug 'elixir-lang/vim-elixir'
" Plug 'jparise/vim-graphql'
" Plug 'pangloss/vim-javascript'
" Plug 'mxw/vim-jsx'
" Plug 'elzr/vim-json'
" Plug 'kchmck/vim-coffee-script'
" " Plug 'styled-components/vim-styled-components'
" Plug 'chrisbra/csv.vim'
" Plug 'leafgarland/typescript-vim' | Plug 'peitalin/vim-jsx-typescript'
" " Plug 'Quramy/tsuquyomi'
" Plug 'shirk/vim-gas'
" Plug 'othree/html5.vim'

" "------------------------------------------
" "--- Utilities
" "-----------------------------------------
" Plug 'diepm/vim-rest-console'
" Plug 'rking/ag.vim'
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Plug 'junegunn/fzf.vim'
" Plug 'editorconfig/editorconfig-vim'
" Plug 'metakirby5/codi.vim', { 'on': 'Codi' }
" " Plug 'craigemery/vim-autotag'
" Plug 'aaronbieber/vim-quicktask'
" Plug 'wakatime/vim-wakatime'
" Plug 'tpope/vim-scriptease'
" "
" "------------------------------------------
" "--- Completion
" "-----------------------------------------
" Plug 'ncm2/ncm2' | Plug 'roxma/nvim-yarp' | Plug 'roxma/vim-hug-neovim-rpc' | Plug 'ncm2/ncm2-path'

" "------------------------------------------
" "--- Disabled
" "-----------------------------------------
" Plug 'easymotion/vim-easymotion' " XXX too annoying
" " Plug 'mbbill/undotree' " XXX barely used
" " Plug 'Raimondi/delimitMate' " XXX this annoys me too much
" " Plug 'xuyuanp/nerdtree-git-plugin' "XXX messy tree
" " Plug 'Valloric/YouCompleteMe', { 'do': './install.py', 'on': [] } " XXX using ncm2 instead
" " Plug 'ternjs/tern_for_vim', { 'do': 'npm i'}
" " Plug 'Shougo/deoplete.nvim' " XXX using ncm2 instead
" Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeToggle', 'NERDTreeFind'] } " XXX using builtin
" "---------------------------------------------------------------"

call plug#end()

"---------------------------------------------------------------"
"--- Indentation
"---------------------------------------------------------------"
filetype plugin indent on " Needs to go before autocmds
syntax enable " Needs to go before autocmds


" "---------------------------------------------------------------"
" "--- autocmd
" "---------------------------------------------------------------"
" if !exists("autocommands_loaded")
"     let autocommands_loaded = 1
"     autocmd CompleteDone * silent! pclose
" endif

" "---------------------------------------------------------------"
" "--- Editor
" "---------------------------------------------------------------"
" set clipboard=unnamed " just too annoying without this
" set mouse=a
" set wrapmargin=0
" " set cursorline " XXX slow
" set regexpengine=1 " TODO really slow without this??
" set relativenumber
" set wildignore=*.keep,*~,*.swp
" set incsearch
" set hlsearch
" set bs=indent,eol,start " Allow backspacing over everything in insert mode
" set laststatus=2
" set dictionary="/usr/dict/words"
" set tags=.tags;
" set nrformats-=octal
" set fillchars=vert:│,fold:·
" set scrolloff=3
" set splitright
" set splitbelow
" set grepprg=rg\ --vimgrep
" set completeopt=noinsert,menuone,noselect " note that must keep noinsert in completeopt, the others is optional
" set wildmenu
" set path+=**


"---------------------------------------------------------------"
"--- Undo
"---------------------------------------------------------------"
" set undolevels=1000
" set undoreload=10000
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

"---------------------------------------------------------------"
"--- Automcomplete / ncm
"---------------------------------------------------------------"
" set shortmess+=c " supress the annoying 'match x of y', 'The only match' and 'Pattern not found' messages
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

"---------------------------------------------------------------"
"--- Appearance
"---------------------------------------------------------------"
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum" " something to do with vim in a terminal
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" color tenderAdam
" color snazzy

if has('gui_running')
    set guioptions=
    " set guifont=Hack\ Regular:h11
    set macligatures
    set guifont=Fira\ Code:h12
    set lines=50 columns=108 linespace=3
    set shellcmdflag=-ic
    let $BASH_ENV = "~/.bash_aliases"
endif
"---------------------------------------------------------------"
"--- Airline
"---------------------------------------------------------------"
" let g:airline_powerline_fonts = 1
" let g:airline_theme = 'tenderAdam'
" let g:airline_theme='snazzy'
" let g:airline#extensions#ale#enabled = 1
" let g:airline#extensions#tagbar#enabled = 0
" let g:airline#extensions#tabline#enabled = 1
" 
" let g:airline#extensions#default#layout = [
"             \ [ 'a', 'b', 'c' ],
"             \ [ 'x', 'z', 'error', 'warning' ]
"             \ ]
" 
" let g:airline_section_b = '%{split(getcwd(), "/")[-1]}' " dont really care for the branch
" " let g:airline_section_c = '%t'
" let g:jsx_ext_required = 0


" "---------------------------------------------------------------"
" "--- Linting
" "---------------------------------------------------------------"
" " let g:ale_lint_on_save = 1
" " let g:ale_lint_on_text_changed = 'normal'
" let g:ale_sign_error = '✘'
" let g:ale_sign_warning = '▹'
" " let g:ale_fix_on_save = 1
" let g:ale_lint_on_insert_leave = 1
" let g:ale_lint_delay = 500
" let g:ale_linters = { 
"             \ 'javascript': ['eslint', 'prettier'],
"             \ 'json': ['prettier'],
"             \ 'graphql': ['prettier'],
"             \}
" let g:ale_fixers = { 
"             \ 'javascript': ['eslint'],
"             \ 'json': ['prettier'],
"             \ 'graphql': ['prettier'],
"             \ 'yml': ['prettier'],
"             \ 'css': ['prettier'],
"             \ 'markdown': ['prettier'],
"             \}

"---------------------------------------------------------------"
"--- NERDTree stuff
"---------------------------------------------------------------"

" autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" let g:NERDTreeWinSize=40 " nice big tree is it's easy to toggle off
" let NERDTreeMinimalUI=1
" let NERDTreeStatusline="%{ getcwd() }"
" let NERDTreeHijackNetrw=1
" let NERDTreeIgnore = ['\.DAT$', '\.LOG1$', '\.LOG1$']
" let NERDTreeIgnore += [
"             \ '\.gif$',
"             \ '\.mp3$',
"             \ '\.flac$',
"             \ '\.ogg$',
"             \ '\.mp4$',
"             \ '\.avi$',
"             \ '.webm$',
"             \ '.mkv$',
"             \ '\.pdf$',
"             \ '\.zip$',
"             \ '\.tar.gz$',
"             \ '\.rar$']

" let g:netrw_liststyle = 3
" let g:netrw_banner = 1

"---------------------------------------------------------------"
"--- Typing stuff
"---------------------------------------------------------------"
" if has("spell")
"     " toggle spelling with F4 key
"     map <F4> :set spell!<CR><Bar>:echo "Spell Check: " . strpart("OffOn", 3 * &spell, 3)<CR>
"
"     " they were using white on white
"     highlight PmenuSel ctermfg=black ctermbg=lightgray
"
"     " limit it to just the top 10 items
"     set sps=best,10
" endif

"---------------------------------------------------------------"
"--- Mappings
"---------------------------------------------------------------"
" let mapleader = ","
let mapleader = " "
" escape key
ino jk <esc>
cno jk <C-c>

nmap <silent> <C-H> :wincmd h<CR>
nmap <silent> <C-J> :wincmd j<CR>
nmap <silent> <C-K> :wincmd k<CR>
nmap <silent> <C-L> :wincmd l<CR>

" " map <C-F> :%s/
" map <C-N> :NERDTreeToggle<CR>
" map <C-P> :Files<CR>
" map <C-\> :Ag!<CR>
" inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
"             \ "\<lt>C-n>" :
"             \ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
"             \ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
"             \ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"
" imap <C-@> <C-Space>

