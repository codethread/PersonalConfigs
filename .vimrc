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
" Plugins {{{
" Plugin Setup {{{
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter *PlugInstall --sync | source $MYVIMRC
endif
" }}}
filetype plugin indent on " Needs to go before autocmds
syntax enable " Needs to go before autocmds
call plug#begin('~/.vim/plugged')
" Editing  {{{
Plug 'christoomey/vim-sort-motion' " use gs
Plug 'danro/rename.vim'
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'
" vim-sneak  {{{
Plug 'justinmk/vim-sneak'
let g:sneak#s_next = 1
let g:sneak#use_ic_scs = 1
" }}}
" vim-pencil  {{{
Plug 'reedes/vim-pencil'
let g:pencil#textwidth = 44
let g:pencil#wrapModeDefault = 'soft'
" }}}
Plug 'tmhedberg/matchit'
Plug 'tomtom/tcomment_vim' " Commenting
Plug 'tpope/vim-abolish' " coerce words such as crs: coerce to snake_case
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-speeddating'
Plug 'ddrscott/vim-window'
" vim-unimpaired  {{{
Plug 'tpope/vim-unimpaired'
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

" }}}
" }}}
" Linting / testing  {{{
" vim-test  {{{
Plug 'janko/vim-test'
function! SkyportTransform(cmd) abort
    let skyportTest = "NODE_ENV=test "
    return skyportTest . a:cmd . ' --file test/setup.js'
endfunction

function! VimVterminal(cmd)
    call term_start([&shell, &shellcmdflag, a:cmd], {
            \ 'term_name': 't:test',
            \ 'vertical': 1,
            \})

    au BufLeave <buffer> wincmd p
    nnoremap <buffer> <Enter> :q<CR>
    redraw
    echo "Press <Enter> to exit test runner terminal (<Ctrl-C> first if command is still running)"
endfunction


let g:test#custom_transformations = {'mocha': function('SkyportTransform')}
let g:test#transformation = 'mocha'

let g:test#custom_strategies = {'vimVterminal': function('VimVterminal')}
let test#strategy = 'vimVterminal'

" }}}
Plug 'ngmy/vim-rubocop'
Plug 'thoughtbot/vim-rspec'
" ale  {{{
Plug 'w0rp/ale' " async linting
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

" }}}
" }}}
" GUI changes  {{{
" indentLine  {{{
Plug 'Yggdroot/indentLine'
let g:indentLine_char = get(g:, 'indentLine_char', '┊')
let g:indentLine_concealcursor = 'niv'
let g:indentLine_conceallevel = 2
let g:indentLine_fileTypeExclude = ['help', 'man', 'startify', 'NERDTree']

" }}}
Plug 'airblade/vim-gitgutter'
Plug 'chrisbra/Colorizer'
Plug 'connorholyday/vim-snazzy'
Plug 'hecal3/vim-leader-guide'
" lightline.vim  {{{
Plug 'itchyny/lightline.vim'

let g:asyncrun_status =""
function! AsyncJobStatus()
    return g:asyncrun_status
endfunction

function! LightlineFilename()
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

function! LightlineFugitive()
    if exists('*fugitive#head')
        let branch = fugitive#head()
        return branch !=# '' ? ''.branch : ''
    endif
    return ''
endfunction

function! LightlineTabMod(n)
  let winnr = tabpagewinnr(a:n)
  return gettabwinvar(a:n, winnr, '&modified') ? '' : gettabwinvar(a:n, winnr, '&modifiable') ? '' : ''
endfunction

function! LightlineTabRead(n)
  let winnr = tabpagewinnr(a:n)
  return gettabwinvar(a:n, winnr, '&readonly') ? '' : ''
endfunction

command! LightlineReload call LightlineReload()

function! LightlineReload()
    call lightline#init()
    call lightline#colorscheme()
    call lightline#update()
endfunction

let g:lightline = { 'colorscheme': 'snazzier' }
let g:lightline.active = { 
            \ 'left': [
            \   [ 
            \     'mode', 
            \     'paste', 
            \     'spell' 
            \   ], 
            \   [ 
            \     'filename', 
            \     'readonlyS', 
            \     'modifiedS', 
            \   ]
            \ ], 
            \ 'right': [
            \   [ 'lineinfo' ],
            \   [ 'filetype' ], 
            \   [ 'asyncJob' ],
            \ ], 
            \}

let g:lightline.inactive = { 
            \ 'left': [ 
            \   [ 'relativepath'], 
            \   [ 'modifiedS' ] 
            \ ],
            \ 'right': [
            \   [ 'lineinfo' ],
            \   [ 'filetype' ],
            \ ],
            \}

let g:lightline.component = { 'readonlyS': '%{&readonly?"":""}', 'modifiedS': '%{&modified?" ":""}', }

let g:lightline.tab_component_function = { 'readonlyS': 'LightlineTabRead', 'modifiedS': 'LightlineTabMod', }

let g:lightline.component_function = { 
            \ 'asyncJob': 'AsyncJobStatus',
            \ 'gitbranch': 'LightlineFugitive',
            \ 'filepath': 'LightlineFilename',
            \ 'filetype': 'LightlineFiletype',
            \}

let g:lightline.tabline = { 
            \ 'left': [
            \   ['tabs']
            \ ],
            \ 'right': [ 
            \   ['gitbranch'],
            \   ['filepath'],
            \ ] 
            \}

let g:lightline.tab = { 
            \ 'active': [ 
            \   'tabnum',
            \   'filename',
            \   'modifiedS',
            \ ], 
            \ 'inactive': [
            \   'tabnum',
            \   'filename',
            \   'modifiedS',
            \ ]
            \}

let g:lightline.separator = { 'left': ' ', 'right': ' ' }
let g:lightline.subseparator = { 'left': '', 'right': '' }
let g:lightline.mode_map = {
            \ 'n' : '',
            \ 'i' : '',
            \ 'R' : '',
            \ 'v' : '',
            \ 'V' : '',
            \ "\<C-v>": '',
            \ 'c' : 'COMMAND',
            \ 's' : 'SELECT',
            \ 'S' : 'S-LINE',
            \ "\<C-s>": 'S-BLOCK',
            \ 't': ' ',
            \ }

" }}}
" goyo.vim  {{{
Plug 'junegunn/goyo.vim'
let g:goyo_width = 120 " default 80
let g:goyo_height = 95 "(default: 85%)
let g:goyo_linenr = 1 " (default: 0)

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

" these are slow
" autocmd! User GoyoEnter nested call <SID>goyo_enter()
" autocmd! User GoyoLeave nested call <SID>goyo_leave()

" }}}
Plug 'kshenoy/vim-signature' " displays marks in column
" tagbar  {{{
Plug 'majutsushi/tagbar'
let g:tagbar_width = 30
let g:tagbar_compact = 0
let g:tagbar_autopreview = 0


" }}}
" }}}
" Languages  {{{
"Plug 'styled-components/vim-styled-components'
" LanguageClient-neovim' {{{
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
let g:LanguageClient_changeThrottle = 0.1 " pauses for x seconds after txt change before post to server
let g:LanguageClient_diagnosticsEnable = 0
let g:LanguageClient_serverCommands = {
            \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
            \ 'css': ['css-languageserver --stdio'],
            \ 'javascript': ['javascript-typescript-stdio'],
            \ 'javascript.jsx': ['javascript-typescript-stdio'],
            \ 'html': ['html-languageserver --stdio'],
            \ 'dockerfile': ['docker-langserver --stdio'],
            \ }

function! LC_maps()
    if has_key(g:LanguageClient_serverCommands, &filetype)
        nnoremap <buffer> <silent> gh :call LanguageClient#textDocument_hover()<cr>
        nnoremap <buffer> <silent> gd :call LanguageClient#textDocument_definition()<CR>
        nnoremap <buffer> <silent> gD :call LanguageClient#textDocument_definition({'gotoCmd': 'vsplit'})<CR>
        nnoremap <buffer> <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
        nnoremap <buffer> <silent> <F5> :call LanguageClient_contextMenu()<cr>
    endif
endfunction

" Debugging
" let g:LanguageClient_loggingLevel = 'TRACE'
" let g:LanguageClient_loggingFile =  glob('~/.local/share/nvim/LanguageClient.log')
" let g:LanguageClient_serverStderr = glob('~/.local/share/nvim/LanguageServer.log')

" Java [gave up]
"java https://github.com/Ruin0x11/intellij-lsp-server
"java https://github.com/eclipse/eclipse.jdt.ls
" \ 'java': ['/usr/local/bin/jdtls'], life's too short to get this to work

" }}}
" vim-polyglot' {{{
Plug 'sheerun/vim-polyglot', { 'tag': 'v3.7.0' } " https://github.com/sheerun/vim-polyglot/issues/387
let g:vim_markdown_conceal = 0
let g:jsx_ext_required = 0
let g:polyglot_disabled = ['typescript']
let g:javascript_plugin_jsdoc = 1
" }}}
Plug 'moll/vim-node'
Plug 'peitalin/vim-jsx-typescript' "|Plug 'Quramy/tsuquyomi' XX play around with this
Plug 'HerringtonDarkholme/yats.vim' " typescript highlighter
Plug 'shirk/vim-gas'

" }}}
" Utilities  {{{
Plug 'skywind3000/asyncrun.vim'
Plug 'tpope/vim-scriptease'
Plug 'wakatime/vim-wakatime'
" previm  {{{
Plug 'kannokanno/previm'
let g:previm_open_cmd = 'open -a "/Applications/Google Chrome.app"'
"
" }}}
" vim-hardtime  {{{
Plug 'takac/vim-hardtime'
let g:hardtime_default_on = 0

" }}}
Plug 'vim-scripts/ParseJSON'
" }}}
" Project management  {{{
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-vinegar'
" vim-rooter  {{{
Plug 'airblade/vim-rooter'
let g:rooter_silent_chdir = 1
let g:rooter_resolve_links = 1
" let g:rooter_manual_only = 1
" exposes FindRootDirectory()
" }}}
Plug 'tpope/vim-projectionist'
Plug 'rking/ag.vim'
" fzf {{{
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
let g:fzf_layout = { 'window': '9split' }
let g:fzf_action = {
            \ 'ctrl-t': 'tab split',
            \ 'ctrl-x': 'split',
            \ 'ctrl-l': 'vsplit' }

let g:fzf_history_dir = '~/.local/share/fzf-history'
let g:fzf_colors = {
            \ 'fg':      ['fg', 'Normal'],
            \ 'bg':      ['bg'],
            \ 'hl':      ['fg', 'Comment'],
            \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
            \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
            \ 'hl+':     ['fg', 'Statement'],
            \ 'info':    ['fg', 'ALEError'],
            \ 'border':  ['fg', 'Boolean'],
            \ 'prompt':  ['fg', 'Boolean'],
            \ 'pointer': ['fg', 'Exception'],
            \ 'marker':  ['fg', 'Keyword'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Comment'] }


command! -bang -nargs=* Ag
            \ call fzf#vim#ag(<q-args>,
            \     <bang>0 ? fzf#vim#with_preview('up:60%')
            \             : fzf#vim#with_preview('right:50%:hidden', '?'),
            \     <bang>0)

command! -bang -nargs=* Rg
            \ call fzf#vim#grep('rg
            \ --column
            \ --line-number
            \ --no-heading
            \ --fixed-strings
            \ --ignore-case
            \ --hidden
            \ --follow
            \ --glob "!.git/*" '.shellescape(<q-args>), 1, <bang>0)

command! VimPlugins call fzf#run(fzf#wrap({ 
            \ 'source': 'fd -t d --hidden --follow -E "{.git,fixtures}" .',
            \ 'dir': '~/.vim',
            \ 'options': '--prompt VimPlugins'
            \ }))

command! Files call fzf#run(fzf#wrap({ 
            \ 'source': 'fd -t f --hidden --follow -E ".git/" .',
            \ 'options': '--prompt '. getcwd() .'/'
            \ }))

command! Projects call fzf#run(fzf#wrap({
            \ 'source': 'fd --type d --exclude "{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}" .',
            \ 'dir': '~',
            \ 'sink': 'ProjectFiles',
            \ 'options': '--prompt Projects: '
            \}))

command! -nargs=1 ProjectFiles call fzf#run(fzf#wrap({
            \ 'source': 'fd -t f --hidden -E ".git/" .',
            \ 'dir': '~/'. <f-args> ,
            \ 'options': '--prompt '. <f-args> .'/'
            \}))
" }}}
Plug 'junegunn/fzf.vim'
" editorconfig-vim  {{{
Plug 'editorconfig/editorconfig-vim'
 
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" }}}
" }}}
" Completion  {{{
" ncm2 {{{
Plug 'ncm2/ncm2' |Plug 'roxma/nvim-yarp' |Plug 'roxma/vim-hug-neovim-rpc' |Plug 'ncm2/ncm2-path'
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


" }}}
" }}}
" Apps  {{{
" vim-quicktask  {{{
Plug 'aaronbieber/vim-quicktask'
" }}}
" calendar.vim  {{{
Plug 'itchyny/calendar.vim'
let g:calendar_google_calendar = 1

" }}}
" vim-orgmode  {{{
Plug 'jceb/vim-orgmode'
" }}}
" vim-startify  {{{
Plug 'mhinz/vim-startify'
" }}}
" vim-rest-console  {{{
Plug 'diepm/vim-rest-console'
" }}}
" codi.vim {{{
Plug 'metakirby5/codi.vim', { 'on': 'Codi' }
let g:codi#rightsplit = 0
let g:codi#rightalign = 0
let g:codi#width = 80

" }}}
" }}}
" Disabled  {{{
"Plug 'Raimondi/delimitMate' " XXX this annoys me too much
"Plug 'Shougo/deoplete.nvim' " XXX using ncm2 instead
"Plug 'Valloric/YouCompleteMe', { 'do': './install.py', 'on': [] } " XXX using ncm2 instead
"Plug 'craigemery/vim-autotag' " XXX maybe if i use other langs
"Plug 'easymotion/vim-easymotion' " XXX too annoying
"Plug 'mbbill/undotree' " XXX barely used
"Plug 'ternjs/tern_for_vim', { 'do': 'npm i'}
"Plug 'vim-airline/vim-airline' | "Plug 'vim-airline/vim-airline-themes' " XXX slow!
"Plug 'wikitopian/hardmode' " XXX hjkl are sometimes really uesful 
"Plug 'xuyuanp/nerdtree-git-plugin' "XXX messy tree
"
" autocmd FileType js UltiSnipsAddFiletypes javascript-react
" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsJumpForwardTrigger="<c-b>"
" let g:UltiSnipsJumpBackwardTrigger="<c-z>"
" }}}
call plug#end()
" }}}
" Autocommands     {{{
if !exists("autocommands_loaded")
    let autocommands_loaded = 1
    autocmd BufEnter * call LC_maps()
          \ | call LayerSet()
          \ | call ncm2#enable_for_buffer() " enable ncm2 for all buffer
    autocmd TextChangedI * call ncm2#auto_trigger()
    autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown
    autocmd CompleteDone * silent! pclose
    autocmd User AsyncRunStop let g:asyncrun_status=""
    autocmd User AsyncRunStart let g:asyncrun_status="❁ "
    " autocmd FileType fzf set laststatus=0 noshowmode noruler
    "             \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
endif
" }}}
" Settings  {{{
set backspace=indent,eol,start              " Allow backspacing over everything in insert mode
set clipboard=unnamed                       " just too annoying without this
set completefunc=LanguageClient#complete
set completeopt=noinsert,menuone,noselect   " note that must keep noinsert in completeopt, the others is optional
" set cursorline                              " XXX slow
set dictionary="/usr/dict/words"
set expandtab                               " tabs are spaces
set fillchars=vert:│,fold:·                 " char between panels
set foldnestmax=3
set grepprg=rg\ --vimgrep
set hidden                                  " allows hiding modified buffers
set hlsearch                                " highlight searches
set ignorecase
set incsearch                               " search as characters are entered
set laststatus=2
set lazyredraw                              " redraw only when we need to.
set mouse=a
set noshowmode
set nrformats-=octal
" set number " XXX challenge
set omnifunc=LanguageClient#complete
set path+=**
set regexpengine=1                          " TODO really slow without this??
" set relativenumber                          " XXX slow
set scrolloff=3
set shiftwidth=2                            " number of spaces in tab when editing
set shortmess+=c
set showtabline=2                           " Show tabline
set signcolumn=yes
set smartcase                               " search ignores case unless capitals present
set splitbelow
set splitright
set sps=best,10                             " spell only shows top 10 results
set tags=.tags;
set tabstop=2                               " number of visual spaces per TAB
set wildignore=*.keep,*~,*.swp
set wildmenu                                " visual autocomplete for command menu
set wrapmargin=0
" highlight PmenuSel ctermfg=black ctermbg=lightgray

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
            \ "\<lt>C-n>" :
            \ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
            \ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
            \ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"

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

let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum" " something to do with vim in a terminal
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

color snazzier

if has('gui_running')
    set guioptions=ec
    " set macligatures XXX SLOW
    set guifont=Fira\ Code:h12
    set termguicolors
    " set guifont=Hack\ Regular:h11
    set lines=50 columns=108 linespace=3
    set shellcmdflag=-ic
    let $BASH_ENV = "~/.bash_aliases"
endif

" }}}
" Mappings {{{
" ! - Overrides {{{
let mapleader = " "
ino jk <esc>
cno jk <C-c>
" move vertically by visual line
nnoremap j gj
nnoremap k gk
nnoremap <C-N> <Plug>VinegarUp
nnoremap <leader><leader> :
" These two allow us to move around lines visually. So if 
" there's a very long line that gets visually wrapped to
" two lines, j won't skip over the 'fake' part of the visual 
" line in favor of the next 'real' line.

map <F4> :set spell!<CR><Bar>:echo "Spell Check: " . strpart("OffOn", 3 * &spell, 3)<CR>
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
map <C-P> :Files<CR>
" map <C-\> :Ag<CR> XXX seems to keep dumping to qf
map \ :Rg<CR>

imap <C-@> <C-Space>
" }}}
" > _ terminals {{{
tnoremap <C-g> <C-W>N
tnoremap <expr> <C-\> "\<c-w>".(winnr() !=# 1 ? ':q' : ':b#')."\n"
" }}}
" @ - macros {{{
let @l='yy^Wwpi^M^[^WW' " send line to next cycled pane
let @r='y^Wwpi^M^[^WW' " send selected region to next cycled pane
let @b='0v/^\n^My^Wwpi^M^[^WW' " send current block to next cycled pane
" }}}
" * - Root Level {{{
let g:lmap = {}
nnoremap <silent> <leader>+ :exe "vertical resize +10"<CR>
nnoremap <silent> <leader>- :exe "vertical resize -10"<CR>

" }}}
" a - AsyncRun {{{
let g:asyncrun_open = 8
let g:lmap.a = { 'name': ' -- Async' }
map <leader>aa :AsyncRun 
map <leader>ac :ccl<CR>
map <leader>ap :AsyncRun <C-r>0<CR>

" }}}
" A - AsyncStop {{{
map <leader>A :AsyncStop<CR>

" }}}
" b - Buffers {{{
let g:lmap.b = { 'name': ' -- Buffers' }
map <leader>bN :enew<CR>
map <leader>bb :SplitPreviousBuffer<CR>
command! SplitPreviousBuffer :vsplit | bprevious
map <leader>bc :BufOnly<CR>
map <leader>bd :DiffSaved<CR>
command! DiffSaved call DiffWithSaved()
map <leader>bk :Bkill!<CR>
map <leader>bl :Buffers<CR>
map <leader>bn :bnext<CR>
map <leader>bp :bprevious<CR>
map <leader>bq :DeleteFileAndBuff<CR> 
command! DeleteFileAndBuff :call delete(expand('%')) | bd
map <leader>br :rename<space>
map <leader>bt :b#<CR>
map <leader>by :YankWoleBuffer<CR>
command! YankWoleBuffer normal gg"*yG

" }}}
" c - Ctags {{{
" let g:lmap.c = { 'name': ' -- Ctags' }
map <leader>c :call GenerateCtags()<CR>

" }}}
" d - Deletions {{{
let g:lmap.d = { 'name': ' -- Delete' }
" nnoremap <leader>d "_d
" vnoremap <leader>d "_d
" nnoremap <leader>D "_D
nnoremap <leader>dd "_d

" }}}
" e - Errors {{{
let g:lmap.e = { 'name': ' -- Errors' }
map <leader>ef :ALEFix<CR>
map <leader>el :lopen<CR>
map <leader>en :ALENextWrap<CR>
map <leader>ep :ALEPreviousWrap<CR>

" }}}
" f - Folds {{{
let g:lmap.f = { 'name': ' -- Folds' }
map <Plug>fold_toggle za
map <leader>ff <Plug>fold_toggle

map <Plug>fold_out zo
map <leader>fo <Plug>fold_out

map <Plug>fold_in zc
map <leader>fi <Plug>fold_in

map <Plug>fold_out_all zR
map <leader>fO <Plug>fold_out_all

map <Plug>fold_in_all zm
map <leader>fI <Plug>fold_in_all

" }}}
" F - File {{{
let g:lmap.F = { 'name': ' -- File' }
map <Plug>test_file :call Testfile(expand('%:p'), getcwd())<CR>
map <leader>FF <Plug>test_file

" }}}
" g - Global {{{
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
command! SourceVimrc write | so ~/.vimrc
map <leader>gv :vsplit ~/.vimrc<CR>

" }}}
" l - Langauge Layer {{{
" see Layer_Functions

" }}}
" o - Help {{{
let g:lmap.h = { 'name': ' -- Help' }
map <leader>hl <Plug>leaderguide-buffer
map <leader>hc :Commands<CR>
map <leader>hi :vert help index<CR>
map <leader>hw :execute 'vert help ' . expand("<cword>")<CR>


" }}}
" n - Nerdtree {{{
" let g:lmap.n = { 'name': ' -- Project' }
map <leader>n :NERDTreeFind<CR>

" }}}
" p - Project {{{
let g:lmap.p = { 'name': ' -- Project' }
map <leader>pg :GFiles?<CR>
map <leader>pm :Marks<CR>
map <leader>pn <C-W>}
map <leader>po :only<CR>
map <leader>pt :TestFile<CR>
map <leader>pw :TestFile '--watch'<CR>
map <leader>pn :TestNearest<CR>

" }}}
" P - Projects {{{
let g:lmap.P = { 'name': ' -- Projects' }
map <Plug>Find_Project :FZF ~<CR>
map <leader>PP <Plug>Find_Project
map <leader>PG :Projects<CR>
map <leader>PS :mksession<CR>

" }}}
" o - Quicktask {{{
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

" }}}
" r - Regex {{{
let g:lmap.r = { 'name': 'global reg' }
map <leader>rr "*


" }}}
" s - Search {{{
let g:lmap.s = { 'name': ' -- Search' }
map <leader>sd :call SearchForDefinition(expand("<cword>"), 1, {})<CR>
map <leader>ss :call SearchForDefinition(expand("<cword>"), 1, {'split': 'vsplit'})<CR>
map <leader>st :CursorInTags<CR>
command! CursorInTags :call fzf#vim#tags(expand("<cword>"))<CR>
map <leader>sw :FindWordUnderCursor<CR>
command! FindWordUnderCursor :call fzf#vim#ag(expand("<cword>"))

" }}}
" t - Terminal {{{
let g:lmap.t = { 'name': ' -- Terminal' }
map <leader>tt :ter ++curwin<CR>
map <leader>tT :vert term<CR>
function! DefaultTerminalOptions(name) 
    let t_options = {
            \ 'term_name': a:name,
            \ 'hidden': 1,
            \ 'term_finish': 'close',
            \}
    return t_options
endfunction

let g:lmap.t.s = { 'name': ' -- Script' }
map <leader>tss :call term_start(
            \ [&shell, &shellcmdflag, "cd $SKYPORT_GRAPHQL_DIR; start_skyport"],
            \ DefaultTerminalOptions('t:skyport'))<CR>:echo 'skyport started'<CR>

map <leader>tsa :call term_start(
            \ [&shell, &shellcmdflag, "cd ~/Service/pages-apps; yarn start:dev"],
            \ DefaultTerminalOptions('t:papps'))<CR>:echo 'papps started'<CR>

map <leader>tsl :call term_start(
            \ [&shell, &shellcmdflag, "cd ~/Service/pages-lib; yarn watch"],
            \ DefaultTerminalOptions('t:papps'))<CR>:echo 'papps started'<CR>

" }}}
" T - Tags {{{
let g:lmap.T = { 'name': ' -- Tags' }
map <leader>Tf :Tags<CR>
map <leader>Tl :ts<CR>
map <leader>Tn :tn<CR>
map <leader>Tp :tp<CR>
map <leader>Tw :ts "<cword>"<CR>

" }}}
" u - Utils {{{
let g:lmap.u = { 'name': ' -- Utils' }
map <leader>uf :call Format()<CR>
map <leader>uj :Json<CR>
command! Json %!python -m json.tool
map <leader>up :PrevimOpen<CR>
map <leader>us :sort<CR>
map <leader>ut :TableFormat<CR>
map <leader>uu :UndotreeToggle<CR>

" }}}
" w - Windows / Panes {{{
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

" }}}
" - - Leaderguide Setup {{{
function! s:my_displayfunc()
    let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '\c<cr>$', '', '')
    let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '^<Plug>', '', '')
endfunction

let g:leaderGuide_displayfunc = [function("s:my_displayfunc")]

call leaderGuide#register_prefix_descriptions("<Space>", "g:lmap")
nnoremap <silent> <leader> :<c-u>LeaderGuide '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>LeaderGuideVisual '<Space>'<CR>
" }}}
" }}}
" Functions  {{{
" ResiseTagBar {{{
function! ResiseTagBar(size)
    let g:tagbar_width = a:size
    exe "TagbarToggle"
    exe "TagbarOpen"
endfunction
" }}}
" StripTrailingWhitespaces {{{
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
" }}}
" DiffWithSaved {{{
function! DiffWithSaved()
    let filetype=&ft
    diffthis
    vnew | r # | normal! 1Gdd
    diffthis
    exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
" }}}
" Format {{{
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
" }}}
" ChangeRootDir {{{
function! ChangeRootDir() 
    let root = FindRootDirectory()
    exe ':cd '.root
endfunction
" }}}
" VimFoldText {{{
function! VimFoldText()
    let s:info = '('.string(v:foldend-v:foldstart).' l)'

    if v:foldlevel == 1
        let s:symbol = ' ● '
    elseif v:foldlevel == 2
        let s:symbol = ' ◇ '
    elseif v:foldlevel == 3
        let s:symbol = ' ▪ '
    endif

    let s:line = getline(v:foldstart)[2:-4]

    return s:symbol . s:line .repeat(' ', 80 - strwidth(s:line) - len(s:info)).s:info
endfunction
" }}}
" Layer_Functions {{{
function! LayerSet()
    let cleanFiletype = substitute(&ft, "\.jsx", "", "")
    let filetype = substitute(cleanFiletype, ".", "\\U\\0", "")
    if exists("*Layer".filetype)
        call CleanMap('l')
        exe "call Layer".filetype."()"
    endif
endfunction

function! CleanMap(letter)
    for key in ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
        exe 'map <leader>'.a:letter.key.' <Nop>'
        exe 'unmap <leader>'.a:letter.key
    endfor
endfunction

function! LayerVim()
    let g:lmap.l = { 'name': ' -- L:Vimscript' }
    map <leader>ll yiwoecho '<C-r>0:'. <C-r>0<C-[>k
endfunction

function! LayerJavascript()
    let g:lmap.l = { 'name': ' -- L:Javascript' }
    map <leader>ll yiwoconsole.log('\n<C-r>0:', <C-r>0);<C-[>k
    map <leader>ld :%s/.*console.log.*\n//g<CR>
endfunction

function! LayerRust()
    let g:lmap.l = { 'name': ' -- L:Rust' }
    map <leader>lf :call TerminalVert("rustc ".expand('%:p')." && ".expand('%:p:r'), expand('%:t'))<CR>
endfunction

function! TerminalVert(cmd, ...)
    let name = get(a:, 1, 'quick')
    call term_start([&shell, &shellcmdflag, a:cmd], {
            \ 'term_name': 't:'.name,
            \ 'vertical': 1,
            \})

    au BufLeave <buffer> wincmd p
    nnoremap <buffer> <Enter> :q<CR>
    redraw
    echo "Press <Enter> to exit terminal (<Ctrl-C> first if command is still running)"
endfunction
" }}}
" }}}
" Debugging  {{{
" set verbose=9
" set verbosefile=~/vim_debug.txt
" }}}
" vim:foldmethod=marker:foldlevel=0 foldtext=VimFoldText()
