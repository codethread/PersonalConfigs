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
Plugin 'tpope/vim-repeat'
Plugin 'kshenoy/vim-signature'
Plugin 'wikitopian/hardmode'
" Plugin 'easymotion/vim-easymotion'
Plugin 'ddrscott/vim-window'
Plugin 'danro/rename.vim'
Plugin 'mbbill/undotree'

"------------------------------------------
"--- Linting / testing
"-----------------------------------------
Plugin 'ngmy/vim-rubocop'
Plugin 'thoughtbot/vim-rspec'
Plugin 'w0rp/ale' " async linting

"------------------------------------------
"--- GUI changes
"-----------------------------------------
Plugin 'airblade/vim-gitgutter'
" Plugin 'Shougo/vimfiler.vim'
Plugin 'scrooloose/nerdtree.git'
Plugin 'xuyuanp/nerdtree-git-plugin'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/denite.nvim'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'gcmt/taboo.vim'
Plugin 'majutsushi/tagbar'
Plugin 'hecal3/vim-leader-guide'
Plugin 'Yggdroot/indentLine'
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/seoul256.vim'

"------------------------------------------
"--- Languages
"-----------------------------------------
" Plugin 'jelera/vim-javascript-syntax' " doesnt seem to do anything?
Plugin 'moll/vim-node'
" Plugin 'tpope/vim-apathy'
Plugin 'sheerun/vim-polyglot'
Plugin 'chrisbra/Colorizer'
Plugin 'elixir-lang/vim-elixir'
Plugin 'godlygeek/tabular'
Plugin 'jparise/vim-graphql'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'elzr/vim-json'
Plugin 'kchmck/vim-coffee-script'
" Plugin 'styled-components/vim-styled-components'
Plugin 'chrisbra/csv.vim'
Plugin 'leafgarland/typescript-vim'
Plugin 'ianks/vim-tsx'
Plugin 'shirk/vim-gas'
" Plugin 'nikvdp/ejs-syntax'
Plugin 'othree/html5.vim'

"------------------------------------------
"--- Color Schemes
"-----------------------------------------
" here use tender-adam in personal config
Plugin 'connorholyday/vim-snazzy'
Plugin 'blueshirts/darcula'
"------------------------------------------
"--- session handling
"-----------------------------------------
" Plugin 'tpope/vim-obsession'

"------------------------------------------
"--- Utilities
"-----------------------------------------
Plugin 'diepm/vim-rest-console'
Plugin 'rking/ag.vim'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'tpope/vim-rhubarb'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'wakatime/vim-wakatime'
Plugin 'Valloric/YouCompleteMe' " snippet engine
Plugin 'SirVer/ultisnips' " snippet tool
Plugin 'honza/vim-snippets' " actual snippet examples
Plugin 'ternjs/tern_for_vim'
Plugin 'metakirby5/codi.vim'
Plugin 'Konfekt/vim-scratchpad'
Plugin 'craigemery/vim-autotag'
" Plugin 'taglist.vim'
Plugin 'aaronbieber/vim-quicktask'

"---------------------------------------------------------------"

" Plugin 'ctrlpvim/ctrlp.vim' " replaced with fzf
" Plugin 'd11wtq/ctrlp_bdelete.vim' " goes with ctrlp

"------------------------------------------
"--- Other stuff
"-----------------------------------------
" Plugin 'christoomey/vim-tmux-navigator'
" Plugin 'craigemery/vim-autotag'
" Plugin 'lchi/vim-toffee'
" Plugin 'mustache/vim-mustache-handlebars'
" Plugin 'sjl/vitality.vim'
" Plugin 'slim-template/vim-slim'
" Plugin 'terryma/vim-multiple-cursors'
" Plugin 'tpope/vim-cucumber'
" Plugin 'tpope/vim-dispatch'
" Plugin 'tpope/vim-endwise'

call vundle#end()

"---------------------------------------------------------------"
"--- Editor
"---------------------------------------------------------------"
" set clipboard=unnamed
set mouse=a
set wrapmargin=0
set cursorline " breaking shit!
set re=1
set relativenumber
set wildignore=*.keep,*~,*.swp
set incsearch
set hlsearch
set bs=indent,eol,start " Allow backspacing over everything in insert mode
set laststatus=2
set dictionary="/usr/dict/words"
autocmd vimenter * set number
set tags=tags;
set nrformats-=octal
let g:EasyMotion_smartcase = 1
let g:vim_markdown_folding_disabled = 1
set fillchars=vert:│,fold:·
set scrolloff=5
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
"--- Appearance
"---------------------------------------------------------------"
syntax enable
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum" " something to do with vim in a terminal
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" color tenderAdam
color snazzy

let g:tagbar_width = 30
let g:tagbar_compact = 0
let g:tagbar_autopreview = 0

" autocmd VimEnter * nested :TagbarOpen

if has('gui_running')
    set guioptions=
    set guifont=Hack\ Regular:h11
    set lines=50 columns=108 linespace=3
    set shellcmdflag=-ic
    let $BASH_ENV = "~/.bash_aliases"
else
    " hi TagbarSignature ctermfg=215
endif



" let g:pencil#textwidth = 44

let g:goyo_width = 120 " default 80
let g:goyo_height = 95 "(default: 85%)
let g:goyo_linenr = 1 " (default: 0)
"---------------------------------------------------------------"
"--- Airline
"---------------------------------------------------------------"
" let g:airline_powerline_fonts = 1
" let g:airline_theme = 'tenderAdam'
let g:airline_theme='snazzy'
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tagbar#enabled = 0
let g:airline#extensions#tabline#enabled = 1

let g:airline#extensions#default#layout = [
            \ [ 'a', 'b', 'c' ],
            \ [ 'x', 'z', 'error', 'warning' ]
            \ ]

let g:airline_section_b = '%{split(getcwd(), "/")[-1]}' " dont really care for the branch
" let g:airline_section_c = '%t'

let g:jsx_ext_required = 1

let g:fzf_layout = { 'down': '~40%' }
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
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_delay = 0
let g:ale_lint_on_insert_leave = 1
let g:ale_linters = { 'javascript': ['eslint'] }
" let g:ale_fix_on_save = 1
let g:ale_fixers = { 'javascript': ['eslint'] }

" {buffer, lines -> filter(lines, 'v:val !=~ ''^\s*//''')}, " removes comments

"---------------------------------------------------------------"
"--- SNippets
"---------------------------------------------------------------"
" autocmd FileType js UltiSnipsAddFiletypes javascript-react

"---------------------------------------------------------------"
"--- Indentation
"---------------------------------------------------------------"
filetype plugin indent on
" set tabstop=2 shiftwidth=2 expandtab
set expandtab
autocmd FileType * setlocal tabstop=4 shiftwidth=4
" autocmd FileType sh setlocal tabstop=2 shiftwidth=2
autocmd FileType javascript.jsx setlocal tabstop=4 shiftwidth=4
" autocmd FileType elixir  setlocal tabstop=4 shiftwidth=4
" autocmd FileType ruby  setlocal tabstop=2 shiftwidth=2
" autocmd FileType yaml  setlocal tabstop=2 shiftwidth=2

autocmd BufNewFile,BufRead *.asm set syntax=nasm

let g:indentLine_char = get(g:, 'indentLine_char', '┊')
let g:indentLine_concealcursor = 'niv'
let g:indentLine_conceallevel = 2
let g:indentLine_fileTypeExclude = ['help', 'man', 'startify', 'NERDTree']
" let g:indentLine_fileTypeExclude = ['help', 'man', 'startify']

"---------------------------------------------------------------"
"--- Utils
"---------------------------------------------------------------"
" let g:UltiSnipsExpandTrigger="<tab>"
" let g:UltiSnipsJumpForwardTrigger="<c-b>"
" let g:UltiSnipsJumpBackwardTrigger="<c-z>"

"---------------------------------------------------------------"
"--- ScratchPads & COdi
"---------------------------------------------------------------"
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_add_preview_to_completeopt = 1
" let g:ycm_key_invoke_completion = '<C-Space>'
let g:ycm_key_invoke_completion = ''
" let g:ycm_key_detailed_diagnostics = '<leader>d'
let g:ycm_key_detailed_diagnostics = ''


let g:codi#rightsplit = 0
let g:codi#rightalign = 0
let g:codi#width = 80

let g:scratchpad_path = '.scratchpads'
nmap dsp <Plug>(ToggleScratchPad)

"---------------------------------------------------------------"
"--- Mappings
"---------------------------------------------------------------"
" let mapleader = ","
let mapleader = " "
" escape key
ino jk <esc>
cno jk <C-c>

let g:UltiSnipsExpandTrigger="<C-b>"
" MAPS ON COMMANDS I DONT LIKE
" map <C-B>
map <C-F> :%s/
" map <C-G>
nmap <silent> <C-H> :wincmd h<CR>
nmap <silent> <C-J> :wincmd j<CR>
nmap <silent> <C-K> :wincmd k<CR>
nmap <silent> <C-L> :wincmd l<CR>
" map <C-M> <Plug>(easymotion-prefix)
map <C-N> :NERDTreeToggle<CR>
" map <C-N> :e.<CR>
map <C-P> :Files<CR>
" map <C-Q>
" map <C-Y>
map <C-\> :Ag!<CR>

" :inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
nmap <Tab> :b#<CR>

"---------------------------------------------------------------"
"--- Dictionary Leader
"---------------------------------------------------------------"
let g:lmap = {}
" 🚀
nnoremap <silent> <Leader>+ :exe "vertical resize +10"<CR>
nnoremap <silent> <Leader>- :exe "vertical resize -10"<CR>

map <Leader>? :Commands<CR>
map \ :Fuzzyag<CR>
map <C-\> :Fuzzyag!<CR>

let g:lmap.b = { 'name': ' -- Buffers' }
map <Leader>bd :DiffSaved<CR>
map <Leader>bl :Buffers<CR>
map <Leader>bn :bnext<CR>
map <Leader>bo :enew<CR>
map <Leader>bp :bprevious<CR>
map <Leader>bs :sbprevious<CR>

let g:lmap.d = { 'name': ' -- Delete' }
" nnoremap <leader>d "_d
" vnoremap <leader>d "_d
" nnoremap <leader>D "_D
nnoremap <leader>dd "_d

let g:lmap.d = { 'name': ' -- File' }
map <Leader>fy :YankWoleBuffer<CR>
map <Leader>ff :call Format()<CR>

let g:lmap.e = { 'name': ' -- Errors' }
map <Leader>ef :ALEFix<CR>
map <Leader>el :lopen<CR>
map <Leader>en :ALENextWrap<CR>
map <Leader>ep :ALEPreviousWrap<CR>

let g:lmap.g = { 'name': ' -- Global' }
map <Leader>gh :History<CR>
map <Leader>gl :set cursorline!<CR>
map <Leader>gn :set nowrap!<CR>
map <Leader>gs :SourceVimrc<CR>
map <leader>g? :help index<CR>
map <leader>gc :ColorToggle<CR>
map <leader>gh <Esc>:call ToggleHardMode()<CR>
map <leader>gr :set relativenumber!<CR>

let g:lmap.l = { 'name': ' -- Layout' }
map <Leader>ls :vsplit<CR>
map <Leader>lt :TagBResise
map <Leader>ll :Goyo<CR>

" let g:lmap.n = { 'name': ' -- Project' }
map <Leader>n :NERDTreeFind<CR>

let g:lmap.p = { 'name': ' -- Project' }
map <Leader>pg :GFiles?<CR>
map <Leader>pm :Marks<CR>
map <Leader>pn <C-W>}
map <Leader>pp :Files<CR>
map <Leader>po :only<CR>

let g:lmap.r = { 'name': 'global reg' }
map <Leader>rr "*

let g:lmap.s = { 'name': ' -- Search' }
map <leader>sd :call SearchForDefinition(expand("<cword>"))<CR>
map <leader>st :CursorInTags<CR>
map <leader>sw :FindWordUnderCursor<CR>

let g:lmap.t = { 'name': ' -- Tags' }
map <leader>tf :Tags<CR>
map <leader>tl :ts<CR>
map <leader>tn :tn<CR>
map <leader>tp :tp<CR>
map <leader>tt :TagbarToggle<CR>
map <leader>tw :ts "<cword>"<CR>

let g:lmap.u = { 'name': ' -- Utils' }
map <leader>us :sort<CR>
map <leader>uu :UndotreeToggle<CR>

let g:lmap.w = { 'name': ' -- Windows' }
map <leader>wa :GoToTab1<CR>
map <leader>ws :GoToTab2<CR>
map <leader>wd :GoToTab3<CR>
map <leader>wf :GoToTab4<CR>
map <leader>wn :tabNext<CR>
map <leader>wN :tabnew<CR>
map <leader>wp :tabprevious<CR>
map <leader>wl :tabs<CR>

let g:lmap.o = { 'name': ' -- Quicktask' }
let g:quicktask_no_mappings = 1
map <Leader>oD  <Plug>TaskComplete
map <Leader>oO  <Plug>AddTaskAbove
map <Leader>oS  <Plug>AddSnipToTask
map <Leader>oa  <Plug>ShowActiveTasksOnly
map <Leader>oc  <Plug>AddChildTask
map <Leader>od  <Plug>MoveTaskDown
map <Leader>ofi <Plug>FindIncompleteTimestamps
map <Leader>on  <Plug>AddNoteToTask
map <Leader>oo  <Plug>AddTaskBelow
map <Leader>os  <Plug>AddNextTimeToTask
map <Leader>ou  <Plug>MoveTaskUp
map <Leader>ov  <Plug>SelectTask
map <Leader>ow  <Plug>ShowWatchedTasksOnly
map <Leader>oy  <Plug>ShowTodayTasksOnly
map <CR>        <Plug>OpenSnipUnderCursor


command! GoToTab1 normal 1gt
command! GoToTab2 normal 2gt
command! GoToTab3 normal 3gt
command! GoToTab4 normal 4gt

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
command! FindWordUnderCursor :call fzf#vim#ag(expand("<cword>"))
command! YankWoleBuffer normal gg"*yG
command! Json %!python -m json.tool
" command! StripTrailingWhitespaces call <SID>StripTrailingWhitespaces()
command! SourceVimrc write | source ~/.vimrc
command! -nargs=1 TagBResise call ResiseTagBar(<f-args>)
command! DiffSaved call DiffWithSaved()
command! CursorInTags :call fzf#vim#tags(expand("<cword>"))<CR>
command! YankWoleBuffer normal gg"*yG

command! -bang -nargs=* Fuzzyag
            \ call fzf#vim#ag(<q-args>,
            \                 <bang>0 ? fzf#vim#with_preview('up:60%')
            \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
            \                 <bang>0)
"---------------------------------------------------------------"
"--- Functions
"---------------------------------------------------------------"
" autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()

"Use TAB to complete when typing words, else inserts TABs as usual.
function! Tab_Or_Complete()
    if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~'^\w'
        return "\<C-N>"
    else
        return "\<Tab>"
    endif
endfunction

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
"---------------------------------------------------------------"
"--- NERdTREE stuff
"---------------------------------------------------------------"
" :let g:vimfiler_as_default_explorer = 1
let g:NERDTreeWinSize=40 " nice big tree is it's easy to toggle off
let NERDTreeMinimalUI=1
let NERDTreeStatusline="%{ getcwd() }"
let NERDTreeHijackNetrw=1
" let g:NERDTreeWinPos = "right"

" closes nerdtree if only open
" autocmd vimenter * NERDTree

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" ignore files */
let NERDTreeIgnore = ['\.DAT$', '\.LOG1$', '\.LOG1$']
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

" \ '\.png$',
" \ '\.jpg$',

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

augroup pencil
    autocmd!
    autocmd FileType markdown,mkd call pencil#init()
    autocmd FileType text         call pencil#init()
augroup END

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
"--- Search for definition */
"---------------------------------------------------------------" */
" not saying this is pretty but at least it works
function! SearchForDefinition(name)
    " try tags first
    " try | exec 'tag ' . a:name | return | catch | silent | endtry

    let quotes = "['" . '"]'
    let langspecific = "\\(from \\|require(\\)"
    let patt = '\<' . a:name . '\>' . '\_[^;]\{-}' . langspecific . quotes . '.\+' . quotes . "[\s;)]*\$"

    if search(patt, 'b') == 0 " look for import
        call search('\(' . a:name . '\|export default\)') " look for export
        return
    endif

    call search(quotes, 'e') " go to file name
    " find out if relative or global
    " js specific index file check
    let workdir = getcwd()


    let isRel = nr2char(strgetchar(getline('.'), col('.'))) =~ '\.'
    if isRel
        exec 'cd '. expand('%:p:h')
    endif

    let options = globpath(expand("<cfile>"), '*',0,1)
    if len(options) == 0 " length = 0 path
        try
            let [file] = getcompletion(expand("<cfile>") . '.', 'file')
        catch
            echo 'node module'
            return
        endtry
    else
        let [file] = getcompletion(expand("<cfile>") . '/index', 'file')
    endif

    if &mod " modified so open split
        exec 'vsplit ' . file
    else
        exec 'e ' . file
    endif

    " put the cd back in order
    exec 'cd '. workdir

    " recurse
    call SearchForDefinition(a:name)
endfunction

"---------------------------------------------------------------" */
"--- Retired */
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


"---------------------------------------------------------------" */
"--- Retired */
"---------------------------------------------------------------" */
" let g:ctrlp_custom_ignore = { */
"                         \ 'dir': '\v[\/](\.git|tmp|node_modules|app_build|build)' */
"                         \} */
" let g:ctrlp_show_hidden = 1 */
" call ctrlp_bdelete#init() */
