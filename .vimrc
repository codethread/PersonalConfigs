if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
"------------------------------------------
"--- Editing
"-----------------------------------------
Plug 'christoomey/vim-sort-motion' " use gs
Plug 'danro/rename.vim'
Plug 'ddrscott/vim-window'
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'
Plug 'justinmk/vim-sneak'
Plug 'kannokanno/previm'
Plug 'kshenoy/vim-signature'
Plug 'reedes/vim-pencil'
Plug 'takac/vim-hardtime'
Plug 'tmhedberg/matchit'
Plug 'tomtom/tcomment_vim' " Commenting
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/ParseJSON'

"------------------------------------------
"--- Linting / testing
"-----------------------------------------
Plug 'ngmy/vim-rubocop'
Plug 'thoughtbot/vim-rspec'
Plug 'w0rp/ale' " async linting

"------------------------------------------
"--- GUI changes
"-----------------------------------------
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
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-vinegar'
" Plug 'plasticboy/vim-markdown'

"------------------------------------------
"--- Languages
"-----------------------------------------
" Plug 'styled-components/vim-styled-components'
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
Plug 'chrisbra/csv.vim'
Plug 'moll/vim-node'
Plug 'peitalin/vim-jsx-typescript' "| Plug 'Quramy/tsuquyomi'
Plug 'sheerun/vim-polyglot'
Plug 'shirk/vim-gas'

"------------------------------------------
"--- Utilities
"-----------------------------------------
Plug 'aaronbieber/vim-quicktask'
Plug 'diepm/vim-rest-console'
Plug 'editorconfig/editorconfig-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'metakirby5/codi.vim', { 'on': 'Codi' }
Plug 'rking/ag.vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'tpope/vim-scriptease'
Plug 'wakatime/vim-wakatime'
"
"------------------------------------------
"--- Completion
"-----------------------------------------
Plug 'ncm2/ncm2' | Plug 'roxma/nvim-yarp' | Plug 'roxma/vim-hug-neovim-rpc' | Plug 'ncm2/ncm2-path'

"------------------------------------------
"--- Disabled
"-----------------------------------------
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
"---------------------------------------------------------------"

call plug#end()

"---------------------------------------------------------------"
"--- Indentation
"---------------------------------------------------------------"
filetype plugin indent on " Needs to go before autocmds
syntax enable " Needs to go before autocmds


"---------------------------------------------------------------"
"--- autocmd
"---------------------------------------------------------------"
if !exists("autocommands_loaded")
    let autocommands_loaded = 1
    autocmd VimEnter * set number
    autocmd BufEnter * 
                \ call ncm2#enable_for_buffer() " enable ncm2 for all buffer
                \ if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

    autocmd CompleteDone * silent! pclose
    autocmd TextChangedI * call ncm2#auto_trigger()

    autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown

    autocmd BufNewFile,BufRead *.{ts,tsx,jsx} 
                \ set filetype=javascript.jsx

    autocmd FileType * 
                \ setlocal tabstop=4 shiftwidth=4 |
                \ call LC_maps()

    autocmd User AsyncRunStop let g:asyncrun_status="✓"
    autocmd User AsyncRunStart let g:asyncrun_status="❁ "
endif


"---------------------------------------------------------------"
"--- Editor
"---------------------------------------------------------------"

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
set relativenumber                          " XXX slow
set scrolloff=3
set shortmess+=c
set splitbelow
set splitright
set tags=.tags;
set wildignore=*.keep,*~,*.swp
set wildmenu
set wrapmargin=0
set showtabline=2  " Show tabline

"---------------------------------------------------------------"
"--- Debug
"---------------------------------------------------------------"
" set verbose=9
" set verbosefile=~/vim_debug.txt

"---------------------------------------------------------------"
"--- Tiny Plugins
"---------------------------------------------------------------"
" let g:EasyMotion_smartcase = 1
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

let g:tagbar_width = 30
let g:tagbar_compact = 0
let g:tagbar_autopreview = 0

let g:indentLine_char = get(g:, 'indentLine_char', '┊')
let g:indentLine_concealcursor = 'niv'
let g:indentLine_conceallevel = 2
let g:indentLine_fileTypeExclude = ['help', 'man', 'startify', 'NERDTree']

let g:previm_open_cmd = 'open -a Safari'
"
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

let g:goyo_width = 120 " default 80
let g:goyo_height = 95 "(default: 85%)
let g:goyo_linenr = 1 " (default: 0)

let g:pencil#textwidth = 44
let g:pencil#wrapModeDefault = 'soft'

let g:codi#rightsplit = 0
let g:codi#rightalign = 0
let g:codi#width = 80

let g:jsx_ext_required = 0

let g:hardtime_default_on = 0

" autocmd FileType js UltiSnipsAddFiletypes javascript-react
" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsJumpForwardTrigger="<c-b>"
" let g:UltiSnipsJumpBackwardTrigger="<c-z>"

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
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

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


let g:LanguageClient_changeThrottle = 0.1 " pauses for 0.3 seconds after txt change before post to server

"---------------------------------------------------------------"
"--- Lsc
"---------------------------------------------------------------"
"java https://github.com/Ruin0x11/intellij-lsp-server
"java https://github.com/eclipse/eclipse.jdt.ls
" \ 'java': ['/usr/local/bin/jdtls'], life's too short to get this to work
" let g:LanguageClient_diagnosticsEnable = 0
let g:LanguageClient_serverCommands = {
            \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
            \ 'css': ['css-languageserver --stdio'],
            \ 'javascript': ['javascript-typescript-stdio'],
            \ 'javascript.jsx': ['javascript-typescript-stdio'],
            \ 'html': ['html-languageserver --stdio'],
            \ 'dockerfile': ['docker-langserver --stdio'],
            \ }
" let g:LanguageClient_loggingLevel = 'TRACE'
" let g:LanguageClient_loggingFile =  glob('~/.local/share/nvim/LanguageClient.log')
" let g:LanguageClient_serverStderr = glob('~/.local/share/nvim/LanguageServer.log')

function! LC_maps()
    if has_key(g:LanguageClient_serverCommands, &filetype)
        nnoremap <buffer> <silent> K :call LanguageClient#textDocument_hover()<cr>
        nnoremap <buffer> <silent> gd :call LanguageClient#textDocument_definition()<CR>
        nnoremap <buffer> <silent> gD :call LanguageClient#textDocument_definition({'gotoCmd': 'vsplit'})<CR>
        nnoremap <buffer> <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
        nnoremap <buffer> <silent> <F5> :call LanguageClient_contextMenu()<cr>
    endif
endfunction


" nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" nnoremap <silent> gh :call LanguageClient#textDocument_hover()<CR>
" nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
"---------------------------------------------------------------"
"--- Appearance
"---------------------------------------------------------------"
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum" " something to do with vim in a terminal
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

let g:onedark_termcolors=256
" color tenderAdam
color snazzy
" colorscheme onedark

if has('gui_running')
    set guioptions=e
    set macligatures
    set guifont=Fira\ Code:h12
    " set guifont=Hack\ Regular:h11
    set lines=50 columns=108 linespace=3
    set shellcmdflag=-ic
    let $BASH_ENV = "~/.bash_aliases"
endif

"---------------------------------------------------------------"
"--- Airline
"---------------------------------------------------------------"
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

"---------------------------------------------------------------"
"--- Lightline
"---------------------------------------------------------------"
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

  let g:lightline = {
              \ 'colorscheme': 'snazzy',
              \ 'active': {
              \   'left': [ [ 'mode', 'paste' ],
              \             [ 'readonly', 'filepath', 'modified' ] ],
              \   'right': [ [ 'lineinfo' ],
              \            [ 'percent' ],
              \            [ 'asyncJob', 'filetype' ] ],
              \ },
              \ 'component_function': {
              \   'asyncJob': 'AsyncJobStatus',
              \   'filepath': 'LightlineFilename',
              \ },
              \ }

  let g:lightline.inactive = {
              \ 'left': [ [ 'filepath', 'modified' ] ],
              \ }

let g:lightline.tabline = {
  \   'left': [ ['tabs'] ],
  \   'right': [ ['close'] ]
  \ }

"---------------------------------------------------------------"
"--- FZF
"---------------------------------------------------------------"
let g:fzf_layout = { 'down': '~20%' }
let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-x': 'split',
            \ 'ctrl-l': 'vsplit' }

let g:fzf_colors = {
            \ 'fg':      ['fg', 'Normal'],
            \ 'bg':      ['bg', 'Normal'],
            \ 'hl':      ['fg', 'Comment'],
            \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
            \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
            \ 'hl+':     ['fg', 'Statement'],
            \ 'info':    ['fg', 'PreProc'],
            \ 'border':  ['fg', 'Ignore'],
            \ 'prompt':  ['fg', 'Conditional'],
            \ 'pointer': ['fg', 'Exception'],
            \ 'marker':  ['fg', 'Keyword'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Comment'] }

"---------------------------------------------------------------"
"--- Linting
"---------------------------------------------------------------"
" let g:ale_lint_on_save = 1
" let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_delay = 100
let g:ale_linters = { 
            \ 'javascript': ['eslint', 'prettier'],
            \ 'json': ['prettier'],
            \ 'graphql': ['prettier'],
            \}
let g:ale_fixers = { 
            \ 'bash': ['/usr/local/bin/shellcheck'],
            \ 'javascript': ['eslint'],
            \ 'json': ['prettier'],
            \ 'graphql': ['prettier'],
            \ 'yml': ['prettier'],
            \ 'css': ['stylelint'],
            \ 'scss': ['stylelint'],
            \ 'markdown': ['prettier'],
            \}

let g:ale_sign_error = '✘'
let g:ale_sign_warning = '▹'
" let g:ale_fix_on_save = 1

" {buffer, lines -> filter(lines, 'v:val !=~ ''^\s*//''')}, " removes comments

"---------------------------------------------------------------"
"--- NERDTree stuff
"---------------------------------------------------------------"
let g:NERDTreeWinSize=40 " nice big tree is it's easy to toggle off
let NERDTreeMinimalUI=1
let NERDTreeStatusline="%{ getcwd() }"
let NERDTreeHijackNetrw=1



let NERDTreeIgnore = ['\.DAT$', '\.LOG1$', '\.LOG1$']
let NERDTreeIgnore += [
            \ '\.swp$',
            \ '.git',
            \ 'node_modules']
let NERDTreeIgnore += [
            \ '\.gif$',
            \ '\.mp3$',
            \ '\.flac$',
            \ '\.ogg$',
            \ '\.mp4$',
            \ '\.avi$',
            \ '.webm$',
            \ '.mkv$',
            \ '\.pdf$',
            \ '\.zip$',
            \ '\.tar.gz$',
            \ '\.rar$']

"---------------------------------------------------------------"
"--- Typing stuff
"---------------------------------------------------------------"
if has("spell")
    " toggle spelling with F4 key
    map <F4> :set spell!<CR><Bar>:echo "Spell Check: " . strpart("OffOn", 3 * &spell, 3)<CR>

    " they were using white on white
    highlight PmenuSel ctermfg=black ctermbg=lightgray

    " limit it to just the top 10 items
    set sps=best,10
endif

"---------------------------------------------------------------" */
"--- Windows and buffers
"---------------------------------------------------------------" */
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


"---------------------------------------------------------------" */
"--- GOYO
"---------------------------------------------------------------" */
function! s:goyo_enter()
    silent !tmux set status off
    silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
    set noshowmode
    set noshowcmd
    " set scrolloff=999
    " Limelight
endfunction

function! s:goyo_leave()
    silent !tmux set status on
    silent !tmux resize-pane -Z
    set showmode
    set showcmd
    " set scrolloff=5
    " Limelight!
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

"---------------------------------------------------------------"
"--- Mappings
"---------------------------------------------------------------"
" let mapleader = ","
let mapleader = " "
" escape key
ino jk <esc>
cno jk <C-c>

" let g:UltiSnipsExpandTrigger="<C-b>" " XXX no longer used

" MAPS ON COMMANDS I DONT LIKE
" map <C-B>
map <C-F> :%s/
" map <C-G>
nmap <silent> <C-H> :wincmd h<CR>
nmap <silent> <C-J> :wincmd j<CR>
nmap <silent> <C-K> :wincmd k<CR>
nmap <silent> <C-L> :wincmd l<CR>
map <C-N> :NERDTreeToggle<CR>
" function! FileFinder(position)
"     " position like Ex Sex Vex
"     echo expand("%:.:h")
"     exec a:position . " ." . expand("%:.:h")
" endfunction
"
" map <C-N> :e %:h<CR>
" " map <C-N> :call FileFinder("Vex")<CR>
" " map <leader>n :Vexplore<CR>
" map <leader>n :Lex<CR>
" map <C-N> :e.<CR>
map <C-P> :Files<CR>
" map <C-Q>
" map <C-Y>
map <C-\> :Ag!<CR>
inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
            \ "\<lt>C-n>" :
            \ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
            \ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
            \ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"
imap <C-@> <C-Space>

" :inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
" nmap <Tab> :b#<CR>
"
"---------------------------------------------------------------"
"--- Dictionary leader
"---------------------------------------------------------------"
let g:lmap = {}
" 🚀
nnoremap <silent> <leader>+ :exe "vertical resize +10"<CR>
nnoremap <silent> <leader>- :exe "vertical resize -10"<CR>

map <leader>? :Commands<CR>
map \ :Fuzzyag<CR>
map <C-\> :Fuzzyag!<CR>

let g:lmap.b = { 'name': ' -- Buffers' }
map <leader>bN :enew<CR>
map <leader>bb :SplitPreviousBuffer<CR>
map <leader>bc :BufOnly<CR>
map <leader>bd :DiffSaved<CR>
map <leader>bl :Buffers<CR>
map <leader>bn :bnext<CR>
map <leader>bp :bprevious<CR>
map <leader>bq :DeleteFileAndBuff<CR> 
map <leader>br :rename<space>
map <leader>bt :b#<CR>
map <leader>by :YankWoleBuffer<CR>

" let g:lmap.c = { 'name': ' -- Ctags' }
map <leader>c :call GenerateCtags()<CR>

let g:lmap.d = { 'name': ' -- Delete' }
" nnoremap <leader>d "_d
" vnoremap <leader>d "_d
" nnoremap <leader>D "_D
nnoremap <leader>dd "_d

let g:lmap.e = { 'name': ' -- Errors' }
map <leader>ef :ALEFix<CR>
map <leader>el :lopen<CR>
map <leader>en :ALENextWrap<CR>
map <leader>ep :ALEPreviousWrap<CR>

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
map <leader>gv :vsplit ~/.vimrc<CR>

function! s:my_displayfunc()
    let g:leaderGuide#displayname =
                \ substitute(g:leaderGuide#displayname, '\c<cr>$', '', '')
    let g:leaderGuide#displayname = 
                \ substitute(g:leaderGuide#displayname, '^<Plug>', '', '')
endfunction
let g:leaderGuide_displayfunc = [function("s:my_displayfunc")]

let g:lmap.l = { 'name': ' -- LSP' }
map <Plug>GoTo-Def :call LanguageClient#textDocument_definition()<CR>
map <leader>ld <Plug>GoTo-Def
" nnoremap <leader>ld :call LanguageClient#textDocument_definition()<CR>
map <leader>lr :call LanguageClient#textDocument_rename()<CR>
map <leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <leader>lt :call LanguageClient#textDocument_typeDefinition()<CR>
map <leader>lx :call LanguageClient#textDocument_references()<CR>
map <leader>la :call LanguageClient_workspace_applyEdit()<CR>
map <leader>lc :call LanguageClient#textDocument_completion()<CR>
map <leader>lh :call LanguageClient#textDocument_hover()<CR>
map <leader>ls :call LanguageClient_textDocument_documentSymbol()<CR>
map <leader>lm :call LanguageClient_contextMenu()<CR>

" let g:lmap.n = { 'name': ' -- Project' }
map <leader>n :NERDTreeFind<CR>

let g:lmap.p = { 'name': ' -- Project' }
map <leader>pg :GFiles?<CR>
map <leader>pm :Marks<CR>
map <leader>pn <C-W>}
map <leader>po :only<CR>
map <leader>pp :Files<CR>

let g:lmap.r = { 'name': 'global reg' }
map <leader>rr "*

let g:lmap.s = { 'name': ' -- Search' }
map <leader>sd :call SearchForDefinition(expand("<cword>"), 1, {})<CR>
map <leader>ss :call SearchForDefinition(expand("<cword>"), 1, {'split': 'vsplit'})<CR>
map <leader>st :CursorInTags<CR>
map <leader>sw :FindWordUnderCursor<CR>

let g:lmap.t = { 'name': ' -- Tags' }
map <leader>tf :Tags<CR>
map <leader>tl :ts<CR>
map <leader>tn :tn<CR>
map <leader>tp :tp<CR>
map <leader>tw :ts "<cword>"<CR>

let g:lmap.u = { 'name': ' -- Utils' }
map <leader>uf :call Format()<CR>
map <leader>uj :Json<CR>
map <leader>up :PrevimOpen<CR>
map <leader>us :sort<CR>
map <leader>ut :TableFormat<CR>
map <leader>uu :UndotreeToggle<CR>

let g:lmap.w = { 'name': ' -- Windows' }
map <leader>wN :tabnew<CR>
map <leader>wl :tabs<CR>
map <leader>wn :tabNext<CR>
map <leader>wo :Goyo<CR>
map <leader>wp :pclose<CR>
map <leader>wp :tabprevious<CR>
map <leader>wT :TagBResise
map <leader>wt :TagbarToggle<CR>
map <leader>ww :vsplit<CR>

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
" map <CR>        <Plug>OpenSnipUnderCursor

" command! GoToTab1 normal 1gt " XXX never used

let g:lmap.z = { 'name': ' -- Folding' }
" let g:leaderGuide_hspace = 6

call leaderGuide#register_prefix_descriptions("<Space>", "g:lmap")
nnoremap <silent> <leader> :<c-u>LeaderGuide '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>LeaderGuideVisual '<Space>'<CR>

"---------------------------------------------------------------"
"--- Macros
"---------------------------------------------------------------"
let @l='yy^Wwpi^M^[^WW' " send line to next cycled pane
let @r='y^Wwpi^M^[^WW' " send selected region to next cycled pane
let @b='0v/^\n^My^Wwpi^M^[^WW' " send current block to next cycled pane

"---------------------------------------------------------------"
"--- Commands
"---------------------------------------------------------------"
"TODO put a check inside DeleteFileAndBuff
command! DeleteFileAndBuff :call delete(expand('%')) | bd
command! FindWordUnderCursor :call fzf#vim#ag(expand("<cword>"))
command! YankWoleBuffer normal gg"*yG
command! Json %!python -m json.tool
command! SourceVimrc write | source ~/.vimrc
command! -nargs=1 TagBResise call ResiseTagBar(<f-args>)
command! DiffSaved call DiffWithSaved()
command! CursorInTags :call fzf#vim#tags(expand("<cword>"))<CR>
command! YankWoleBuffer normal gg"*yG
command! SplitPreviousBuffer :vsplit | bprevious

command! -bang -nargs=* Fuzzyag
            \ call fzf#vim#ag(<q-args>,
            \                 <bang>0 ? fzf#vim#with_preview('up:60%')
            \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
            \                 <bang>0)

command! -bang -nargs=* Find
            \ call fzf#vim#grep('rg
            \ --column
            \ --line-number
            \ --no-heading
            \ --fixed-strings
            \ --ignore-case
            \ --hidden
            \ --follow
            \ --glob "!.git/*"
            \ --color "always" '.shellescape(<q-args>), 1, <bang>0)

"---------------------------------------------------------------"
"--- Functions
"---------------------------------------------------------------"
"Use TAB to complete when typing words, else inserts TABs as usual.
" function! Tab_Or_Complete()
"     if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~'^\w'
"         return "\<C-N>"
"     else
"         return "\<Tab>"
"     endif
" endfunction

function! ResiseTagBar(size)
    let g:tagbar_width = a:size
    exe "TagbarToggle"
    exe "TagbarOpen"
endfunction

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

function! DiffWithSaved()
    let filetype=&ft
    diffthis
    vnew | r # | normal! 1Gdd
    diffthis
    exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction

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
