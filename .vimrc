"
"      ___       _______       ___      .___  ___.  __     _______.   
"     /   \     |       \     /   \     |   \/   | (_ )   /       |   
"    /  ^  \    |  .--.  |   /  ^  \    |  \  /  |  |/   |   (----`   
"   /  /_\  \   |  |  |  |  /  /_\  \   |  |\/|  |        \   \       
"  /  _____  \  |  '--'  | /  _____  \  |  |  |  |    .----)   |      
" /__/     \__\ |_______/ /__/     \__\ |__|  |__|    |_______/       
" ____    ____  __  .___  ___. .______        ______ 
" \   \  /   / |  | |   \/   | |   _  \      /      |
"  \   \/   /  |  | |  \  /  | |  |_)  |    |  ,----'
"   \      /   |  | |  |\/|  | |      /     |  |     
"    \    /    |  | |  |  |  | |  |\  \----.|  `----.
"     \__/     |__| |__|  |__| | _| `._____| \______|
" 

"""""""""""""
"  Plugins  "
"""""""""""""
"" Setup
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

"" Editing
Plug 'christoomey/vim-sort-motion' " use gs
Plug 'danro/rename.vim'
Plug 'ddrscott/vim-window'
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'
Plug 'justinmk/vim-sneak'
Plug 'kshenoy/vim-signature'
Plug 'reedes/vim-pencil'
Plug 'tmhedberg/matchit'
Plug 'tomtom/tcomment_vim' " Commenting
Plug 'tpope/vim-abolish' " coerce words such as crs: coerce to snake_case
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/ParseJSON'

"" Linting / testing
Plug 'ngmy/vim-rubocop'
Plug 'thoughtbot/vim-rspec'
Plug 'w0rp/ale' " async linting
Plug 'janko/vim-test'


"" GUI changes
Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeToggle', 'NERDTreeFind'] } | Plug 'ryanoasis/vim-devicons'
Plug 'Shougo/denite.nvim'
Plug 'Yggdroot/indentLine'
Plug 'airblade/vim-gitgutter'
Plug 'chrisbra/Colorizer'
Plug 'connorholyday/vim-snazzy'
Plug 'gcmt/taboo.vim'
Plug 'hecal3/vim-leader-guide'
Plug 'itchyny/lightline.vim'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/goyo.vim'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-vinegar'

"" Languages
" Plug 'styled-components/vim-styled-components'
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
Plug 'chrisbra/csv.vim'
Plug 'moll/vim-node'
Plug 'peitalin/vim-jsx-typescript' "| Plug 'Quramy/tsuquyomi'
Plug 'sheerun/vim-polyglot'
Plug 'shirk/vim-gas'
Plug 'plasticboy/vim-markdown'

"" Utilities
Plug 'aaronbieber/vim-quicktask'
Plug 'diepm/vim-rest-console'
Plug 'metakirby5/codi.vim', { 'on': 'Codi' }
Plug 'skywind3000/asyncrun.vim'
Plug 'tpope/vim-scriptease'
Plug 'wakatime/vim-wakatime'
Plug 'kannokanno/previm'
Plug 'takac/vim-hardtime'

"" Projects
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-projectionist'
Plug 'rking/ag.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'editorconfig/editorconfig-vim'

"" Completion
Plug 'ncm2/ncm2' | Plug 'roxma/nvim-yarp' | Plug 'roxma/vim-hug-neovim-rpc' | Plug 'ncm2/ncm2-path'

"" Disabled
" Plug 'Raimondi/delimitMate' " XXX this annoys me too much
" Plug 'Shougo/deoplete.nvim' " XXX using ncm2 instead
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py', 'on': [] } " XXX using ncm2 instead
" Plug 'craigemery/vim-autotag' " XXX maybe if i use other langs
" Plug 'easymotion/vim-easymotion' " XXX too annoying
" Plug 'mbbill/undotree' " XXX barely used
" Plug 'ternjs/tern_for_vim', { 'do': 'npm i'}
" Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes' " XXX slow!
" Plug 'wikitopian/hardmode' " XXX hjkl are sometimes really uesful 
" Plug 'xuyuanp/nerdtree-git-plugin' "XXX messy tree

call plug#end()

""""""""""""""""""""
"  Autocommands    "
""""""""""""""""""""
filetype plugin indent on " Needs to go before autocmds
syntax enable " Needs to go before autocmds

if !exists("autocommands_loaded")
    let autocommands_loaded = 1

    autocmd BufEnter * call LC_maps()
                \ | call ncm2#enable_for_buffer() " enable ncm2 for all buffer
                \ | if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) q endif

    autocmd TextChangedI * call ncm2#auto_trigger()

    autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown
    autocmd BufNewFile,BufRead *.{ts,tsx,jsx} set filetype=javascript.jsx

    autocmd FileType * setlocal tabstop=4 shiftwidth=4 

    autocmd FileType javascript,javascript.jsx 
                \ setlocal foldmethod=syntax |
                \ normal zR

    autocmd CompleteDone * silent! pclose
    autocmd User AsyncRunStop let g:asyncrun_status="✓"
    autocmd User AsyncRunStart let g:asyncrun_status="❁ "
endif

""""""""""""""""""""
"  Settings "
""""""""""""""""""""
"" Vim Settings
set backspace=indent,eol,start              " Allow backspacing over everything in insert mode
set clipboard=unnamed                       " just too annoying without this
set omnifunc=LanguageClient#complete
set completefunc=LanguageClient#complete
set completeopt=noinsert,menuone,noselect   " note that must keep noinsert in completeopt, the others is optional
" set cursorline                              " XXX slow
set dictionary="/usr/dict/words"
set expandtab
set fillchars=vert:│,fold:·                 " char between panels
set grepprg=rg\ --vimgrep
set hlsearch
set incsearch
set laststatus=2
set mouse=a
set nrformats-=octal
set path+=**
set regexpengine=1                          " TODO really slow without this??
" set relativenumber                          " XXX slow
set scrolloff=3
set shortmess+=c
set splitbelow
set splitright
set tags=.tags;
set wildignore=*.keep,*~,*.swp
set wildmenu
set wrapmargin=0
set showtabline=1  " Show tabline
set signcolumn=yes
set ignorecase
set smartcase " search ignores case unless capitals present
set foldnestmax=3
set noshowmode
" set number " XXX challenge
set hidden " allows hiding modified buffers
" set foldlevelstart=20 " useful for making sure all folds are expanded on
" opening
" set foldminlines=5

"" Spelling
if has("spell")
    " toggle spelling with F4 key
    map <F4> :set spell!<CR><Bar>:echo "Spell Check: " . strpart("OffOn", 3 * &spell, 3)<CR>

    " they were using white on white
    highlight PmenuSel ctermfg=black ctermbg=lightgray

    " limit it to just the top 10 items
    set sps=best,10
endif

"" Undo
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

""""""""""""""""""""
"  Appearance "
""""""""""""""""""""
"" Theme
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum" " something to do with vim in a terminal
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
color snazzy

"" GUI
if has('gui_running')
    set guioptions=e
    set macligatures
    set guifont=Fira\ Code:h12
    " set guifont=Hack\ Regular:h11
    set lines=50 columns=108 linespace=3
    set shellcmdflag=-ic
    let $BASH_ENV = "~/.bash_aliases"
endif
"" Airline
" let g:airline_powerline_fonts = 1
" let g:airline_theme = 'tenderAdam'
" let g:airline_theme='onedark'

" let g:airline_theme='snazzy'
" let g:airline#extensions#ale#enabled = 1
" let g:airline#extensions#tagbar#enabled = 0
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#default#layout = [
"             \ [ 'a', 'b', 'c' ],
"             \ [ 'x', 'z', 'error', 'warning' ]
"             \ ]
" let g:airline_section_b = '%{split(getcwd(), "/")[-1]}' " dont really care for the branch
" " let g:airline_section_c = '%t'

"" Lightline
let g:asyncrun_status = "" 
function! AsyncJobStatus()
    return g:asyncrun_status
endfunction

function! LightlineFilename()
    " let root = fnamemodify(get(b:, 'git_dir'), ':h')
    let root = fnamemodify(get(b:, 'gitbranch_path'), ':h:h')
    let path = expand('%:p')
    if path[:len(root)-1] ==# root
        return path[len(root)+1:]
    endif
    return expand('%')
endfunction

function! LightlineFiletype()
    return winwidth(0) > 89 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction


  let g:lightline = {
              \ 'colorscheme': 'snazzy',
              \ 'active': {
              \   'left': [ [ 'mode', 'paste' ],
              \             [ 'readonly', 'filepath', 'modified' ] ],
              \   'right': [ [ 'lineinfo' ],
              \            [ 'filetype' ],
              \            [ 'asyncJob' ] ],
              \ },
              \ 'component_function': {
              \   'asyncJob': 'AsyncJobStatus',
              \   'filepath': 'LightlineFilename',
              \   'filetype': 'LightlineFiletype',
              \ },
              \ }

  let g:lightline.inactive = {
              \ 'left': [ [ 'filepath', 'modified' ] ],
              \ }


" let g:lightline.tabline = {
"   \   'left': [ ['tabs'] ],
"   \   'right': [ ['close'] ]
"   \ }

""""""""""""""""""""
"  Mappings"
""""""""""""""""""""
let mapleader = " "
ino jk <esc>
cno jk <C-c>

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
map <C-N> :NERDTreeToggle<CR>
map <C-P> :Files<CR>
map <C-\> :Fuzzyag!<CR>
map \ :Fuzzyag<CR>

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
            \ "\<lt>C-n>" :
            \ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
            \ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
            \ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"
imap <C-@> <C-Space>
" :inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>

"" macros
let @l='yy^Wwpi^M^[^WW' " send line to next cycled pane
let @r='y^Wwpi^M^[^WW' " send selected region to next cycled pane
let @b='0v/^\n^My^Wwpi^M^[^WW' " send current block to next cycled pane

""""""""""""""""""
"  Leader Guide  "
""""""""""""""""""
"" * - Root Level
let g:lmap = {}
nnoremap <silent> <leader>+ :exe "vertical resize +10"<CR>
nnoremap <silent> <leader>- :exe "vertical resize -10"<CR>

"" b - Buffers
let g:lmap.b = { 'name': ' -- Buffers' }
map <leader>bN :enew<CR>
map <leader>bb :SplitPreviousBuffer<CR>
command! SplitPreviousBuffer :vsplit | bprevious
map <leader>bc :BufOnly<CR>
map <leader>bd :DiffSaved<CR>
command! DiffSaved call DiffWithSaved()
map <leader>bl :Buffers<CR>
map <leader>bn :bnext<CR>
map <leader>bp :bprevious<CR>
map <leader>bq :DeleteFileAndBuff<CR> 
command! DeleteFileAndBuff :call delete(expand('%')) | bd
map <leader>br :rename<space>
map <leader>bt :b#<CR>
map <leader>by :YankWoleBuffer<CR>
command! YankWoleBuffer normal gg"*yG

"" c - Ctags
" let g:lmap.c = { 'name': ' -- Ctags' }
map <leader>c :call GenerateCtags()<CR>

"" d - Deletions
let g:lmap.d = { 'name': ' -- Delete' }
" nnoremap <leader>d "_d
" vnoremap <leader>d "_d
" nnoremap <leader>D "_D
nnoremap <leader>dd "_d

"" e - Errors
let g:lmap.e = { 'name': ' -- Errors' }
map <leader>ef :ALEFix<CR>
map <leader>el :lopen<CR>
map <leader>en :ALENextWrap<CR>
map <leader>ep :ALEPreviousWrap<CR>

"" f - Folds
let g:lmap.f = { 'name': ' -- Folds' }
map <Plug>fold_out zo
map <leader>fo <Plug>fold_out

map <Plug>fold_in zc
map <leader>fi <Plug>fold_in

map <Plug>fold_out_all zR
map <leader>fO <Plug>fold_out_all

map <Plug>fold_in_all zm
map <leader>fI <Plug>fold_in_all

"" F - File
let g:lmap.F = { 'name': ' -- File' }
" map <Plug>fold_out :call TestFile()<CR>
map <Plug>test_file :call TestFile(expand('%:p'), getcwd())<CR>
map <leader>FF <Plug>test_file

"" g - Global
let g:lmap.g = { 'name': ' -- Global' }
map <leader>g? :help index<CR>
map <leader>gc :ColorToggle<CR>
map <leader>gg :PencilToggle<CR>
map <leader>gh :History<CR>
map <leader>gh :HardTimeToggle<CR>
map <leader>gl :set cursorline!<CR>
map <leader>gn :set nowrap!<CR>
map <leader>gp :call pencil#init()<CR>
map <leader>gr :set relativenumber!<CR>
map <leader>gs :SourceVimrc<CR>
command! SourceVimrc write | source ~/.vimrc
map <leader>gv :vsplit ~/.vimrc<CR>

"" l - Log
let g:lmap.l = { 'name': ' -- Log' }
map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
map <leader>ld :%s/.*console.log.*\n//g<CR>


"" o - Help
let g:lmap.h = { 'name': ' -- Help' }
map <leader>hl <Plug>leaderguide-buffer
map <leader>hc :Commands<CR>
map <leader>hi :vert help index<CR>
map <leader>hw :execute 'vert help ' . expand("<cword>")<CR>


"" n - Nerdtree
" let g:lmap.n = { 'name': ' -- Project' }
map <leader>n :NERDTreeFind<CR>

"" p - Project
let g:lmap.p = { 'name': ' -- Project' }
map <leader>pg :GFiles?<CR>
map <leader>pm :Marks<CR>
map <leader>pn <C-W>}
map <leader>po :only<CR>
map <leader>pp :AV<CR>
map <leader>pt :Files<CR>

"" o - Quicktask
let g:lmap.o = { 'name': ' -- Quicktask' }
let g:quicktask_no_mappings = 1
map <leader>oD  <Plug>TaskComplete
map <leader>oO  <Plug>AddTaskAbove
map <leader>oS  <Plug>AddSnipToTask
map <leader>oa  <Plug>ShowActiveTasksOnly
map <leader>oc  <Plug>AddChildTask
map <leader>od  <Plug>MoveTaskDown
map <leader>ofi <Plug>FindIncompleteTimestamps
map <leader>on  <Plug>AddNoteToTask
map <leader>oo  <Plug>AddTaskBelow
map <leader>os  <Plug>AddNextTimeToTask
map <leader>ou  <Plug>MoveTaskUp
map <leader>ov  <Plug>SelectTask
map <leader>ow  <Plug>ShowWatchedTasksOnly
map <leader>oy  <Plug>ShowTodayTasksOnly

" let g:lmap.z = { 'name': ' -- Folding' }

"" r - Regex
let g:lmap.r = { 'name': 'global reg' }
map <leader>rr "*

"" s - Search
let g:lmap.s = { 'name': ' -- Search' }
map <leader>sd :call SearchForDefinition(expand("<cword>"), 1, {})<CR>
map <leader>ss :call SearchForDefinition(expand("<cword>"), 1, {'split': 'vsplit'})<CR>
map <leader>st :CursorInTags<CR>
command! CursorInTags :call fzf#vim#tags(expand("<cword>"))<CR>
map <leader>sw :FindWordUnderCursor<CR>
command! FindWordUnderCursor :call fzf#vim#ag(expand("<cword>"))

"" t - Tags
let g:lmap.t = { 'name': ' -- Tags' }
map <leader>tf :Tags<CR>
map <leader>tl :ts<CR>
map <leader>tn :tn<CR>
map <leader>tp :tp<CR>
map <leader>tw :ts "<cword>"<CR>

"" u - Utils
let g:lmap.u = { 'name': ' -- Utils' }
map <leader>uf :call Format()<CR>
map <leader>uj :Json<CR>
command! Json %!python -m json.tool
map <leader>up :PrevimOpen<CR>
map <leader>us :sort<CR>
map <leader>ut :TableFormat<CR>
map <leader>uu :UndotreeToggle<CR>

"" w - Windows / Panes
let g:lmap.w = { 'name': ' -- Windows' }
map <leader>wN :tabnew<CR>
map <leader>wl :tabs<CR>
map <leader>wn :tabNext<CR>
map <leader>wo :Goyo<CR>
map <leader>wp :pclose<CR>
map <leader>wp :tabprevious<CR>
map <leader>wT :TagBResise
command! -nargs=1 TagBResise call ResiseTagBar(<f-args>)
map <leader>wt :TagbarToggle<CR>
map <leader>ww :vsplit<CR>

"" Leaderguide Setup
function! s:my_displayfunc()
    let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '\c<cr>$', '', '')
    let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '^<Plug>', '', '')
endfunction

let g:leaderGuide_displayfunc = [function("s:my_displayfunc")]

call leaderGuide#register_prefix_descriptions("<Space>", "g:lmap")
nnoremap <silent> <leader> :<c-u>LeaderGuide '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>LeaderGuideVisual '<Space>'<CR>

""""""""""""""
"  Functions "
""""""""""""""
"" ResiseTagBar
function! ResiseTagBar(size)
    let g:tagbar_width = a:size
    exe "TagbarToggle"
    exe "TagbarOpen"
endfunction

"" StripTrailingWhitespaces
" via: http://rails-bestpractices.com/posts/60-remove-trailing-whitespace
" Strip trailing whitespace
function! StripTrailingWhitespaces()
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

"" DiffWithSaved
function! DiffWithSaved()
    let filetype=&ft
    diffthis
    vnew | r # | normal! 1Gdd
    diffthis
    exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction

"" Format
function! Format()
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    normal gg=G
    call cursor(l, c)
endfunction

function! TestFile(file, project)
    let skyportTest = "NODE_ENV=test npx mocha --watch test/setup.js "

    " call term_start([&shell, &shellcmdflag, "echo ".a:file])
    let options = {
        \ 'term_name': 't:lovely test',
        \ 'norestore': 'true',
        \ }
    call term_start([&shell, &shellcmdflag, "" . skyportTest . a:file], options)
endfunction
"Use TAB to complete when typing words, else inserts TABs as usual.
" function! Tab_Or_Complete()
"     if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~'^\w'
"         return "\<C-N>"
"     else
"         return "\<Tab>"
"     endif
" endfunction


""""""""""""""""""""
"  Plugin Configs  "
""""""""""""""""""""
"" EditorConfig
" let g:EasyMotion_smartcase = 1
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

"" Tagbar
let g:tagbar_width = 30
let g:tagbar_compact = 0
let g:tagbar_autopreview = 0

"" Indentline
let g:indentLine_char = get(g:, 'indentLine_char', '┊')
let g:indentLine_concealcursor = 'niv'
let g:indentLine_conceallevel = 2
let g:indentLine_fileTypeExclude = ['help', 'man', 'startify', 'NERDTree']

"" Previm -- open md in safari
let g:previm_open_cmd = 'open -a "/Applications/Google Chrome.app"'
"
"" Vim markdown -- needs addressing for speed
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal = 0

" csharp=cs means csharp highlights as cs
let g:vim_markdown_fenced_languages = [
            \ 'coffee',
            \ 'css',
            \ 'erb=eruby',
            \ 'javascript',
            \ 'typescript',
            \ 'js=javascript',
            \ 'json=javascript',
            \ 'ruby',
            \ 'sass',
            \ 'xml'
            \ ]

"" Pencil -- writing n stuff
let g:pencil#textwidth = 44
let g:pencil#wrapModeDefault = 'soft'

"" Codi -- awesome js REPL
let g:codi#rightsplit = 0
let g:codi#rightalign = 0
let g:codi#width = 80

"" Javascript.jsx
let g:jsx_ext_required = 0

"" Hardtime
let g:hardtime_default_on = 0

"" Sneak
let g:sneak#s_next = 1
let g:sneak#use_ic_scs = 1

"" UtilSnips
" autocmd FileType js UltiSnipsAddFiletypes javascript-react
" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsJumpForwardTrigger="<c-b>"
" let g:UltiSnipsJumpBackwardTrigger="<c-z>"

"" NCM2
" au User Ncm2Plugin call ncm2#register_source({
"             \ 'name' : 'css',
"             \ 'priority': 9, 
"             \ 'subscope_enable': 1,
"             \ 'scope': ['css','scss'],
"             \ 'mark': 'css',
"             \ 'word_pattern': '[\w\-]+',
"             \ 'complete_pattern': ':\s*',
"             \ 'on_complete': ['ncm2#on_complete#omni', 'csscomplete#CompleteCSS'],
"             \ })

"" Windows and buffers
" Unimpaired mapping
nnoremap ]r :<C-U>call window#rotate(-1 * v:count1)<cr>
nnoremap [r :<C-U>call window#rotate(1 * v:count1)<cr>

" Improved window rotate to work with all layouts
nmap <C-w>r ]r
nmap <C-w><C-r> ]r
" Improve window exchange to work with all layouts
nnoremap <C-w>x :<C-U>call window#exchange(v:count)<cr>
nnoremap <C-w><c-x> :<C-U>call window#exchange(v:count)<cr>

" [g]lue windows together.
"    l = glue to right side
"    h = glue to left side
"    j = glue to bottom
"    k = glue to top
"
" `normal! 100zh` scrolls window contents into view since it gets messy when
" narrower window tries refocuses its cursor.
nnoremap <C-w>gl :<C-U>call window#join('rightbelow vsplit', v:count) <BAR>normal! 100zh<CR>
nnoremap <C-w>gh :<C-U>call window#join('leftabove vsplit', v:count) <BAR>normal! 100zh<CR>
nnoremap <C-w>gj :<C-U>call window#join('belowright split', v:count) <BAR>normal! 100zh<CR>
nnoremap <C-w>gk :<C-U>call window#join('aboveleft split', v:count) <BAR>normal! 100zh<CR>

"" Rooter
let g:rooter_silent_chdir = 1

"" Vim-Test
function! SkyportTransform(cmd) abort
    let skyportTest = "NODE_ENV=test npx mocha test/setup.js "
    return skyportTest . a:cmd
endfunction

let g:test#custom_transformations = {'mocha': function('SkyportTransform')}
let g:test#transformation = 'mocha'

""""""""""""""""""""
"  Source Settings "
""""""""""""""""""""
so ~/PersonalConfigs/vim/settings/colors.vim
so ~/PersonalConfigs/vim/settings/vim_fold.vim
so ~/PersonalConfigs/vim/settings/nerd_tree.vim
so ~/PersonalConfigs/vim/settings/ale.vim
so ~/PersonalConfigs/vim/settings/LanguageClient_neovim.vim
so ~/PersonalConfigs/vim/settings/fzf.vim
so ~/PersonalConfigs/vim/settings/goyo.vim
""""""""""""""""""""
"  Debugging "
""""""""""""""""""""
" set verbose=9
" set verbosefile=~/vim_debug.txt

