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
Plugin 'scrooloose/nerdtree.git'
Plugin 'xuyuanp/nerdtree-git-plugin'
" Plugin 'Shougo/vimfiler.vim'
" Plugin 'Shougo/unite.vim'
Plugin 'Shougo/denite.nvim'
Plugin 'tpope/vim-fugitive'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'gcmt/taboo.vim'
Plugin 'majutsushi/tagbar'
Plugin 'hecal3/vim-leader-guide'
Plugin 'Yggdroot/indentLine'

"------------------------------------------
"--- Languages
"-----------------------------------------
" Plugin 'jelera/vim-javascript-syntax' " doesnt seem to do anything?
Plugin 'sheerun/vim-polyglot'
Plugin 'chrisbra/Colorizer'
Plugin 'elixir-lang/vim-elixir'
Plugin 'godlygeek/tabular'
Plugin 'jparise/vim-graphql'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
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
:set dictionary="/usr/dict/words"
autocmd vimenter * set number
set tags=tags;
set nrformats-=octal
let g:EasyMotion_smartcase = 1
let g:vim_markdown_folding_disabled = 1
set fillchars=vert:│,fold:·
"---------------------------------------------------------------"
"--- Undo
"---------------------------------------------------------------"
set undodir=~/.vim/undo
set undofile
set undolevels=1000
set undoreload=10000
"---------------------------------------------------------------"
"--- Appearance
"---------------------------------------------------------------"
syntax enable
color tenderAdam
let g:tagbar_width = 30
let g:tagbar_compact = 0
let g:tagbar_autopreview = 0

highlight TagbarSignature ctermfg=215
" autocmd VimEnter * nested :TagbarOpen

" set guioptions-=e
" set guifont=Source\ Code\ Pro\ Italic\ for\ Powerline\ 11

" let g:pencil#textwidth = 44
"---------------------------------------------------------------"
"--- Airline
"---------------------------------------------------------------"
let g:airline_powerline_fonts = 1
let g:airline_theme = 'tenderAdam'
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1

let g:airline_section_b = '%{split(getcwd(), "/")[-1]}' " dont really care for the branch
" let g:airline_section_c = '%t'

let g:airline_section_x = '%{bufnr("%")}'
let g:airline_section_y = '%y'

let g:jsx_ext_required = 1
set statusline+=%#warningmsg#
set statusline+=%*
let g:fzf_layout = { 'down': '~40%' }
let g:fzf_action = { 'ctrl-l': 'vsplit' }
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
let g:ale_fix_on_save = 1
let b:ale_fixers = {
                        \   'javascript': [
                        \       'eslint',
                        \   ],
                        \}

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
" autocmd FileType * setlocal tabstop=2 shiftwidth=2
" autocmd FileType sh setlocal tabstop=2 shiftwidth=2
" autocmd FileType javascript setlocal tabstop=2 shiftwidth=2
" autocmd FileType elixir  setlocal tabstop=4 shiftwidth=4
" autocmd FileType ruby  setlocal tabstop=2 shiftwidth=2
" autocmd FileType yaml  setlocal tabstop=2 shiftwidth=2

autocmd BufNewFile,BufRead *.asm set syntax=nasm

let g:indentLine_char = get(g:, 'indentLine_char', '┊')
let g:indentLine_concealcursor = 'niv'
let g:indentLine_conceallevel = 2
let g:indentLine_fileTypeExclude = ['help', 'man', 'startify', 'NERDTree']

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

" let g:lmap.d = { 'name': ' -- Delete' }
" nnoremap <leader>d "_d
" vnoremap <leader>d "_d
" nnoremap <leader>D "_D
" nnoremap <leader>dd "_dd

let g:lmap.b = { 'name': ' -- Buffers' }
map <Leader>bd :DiffSaved<CR>
map <Leader>bf :Format<CR>
map <Leader>bl :Buffers<CR>
map <Leader>bn :bnext<CR>
map <Leader>bo :enew<CR>
map <Leader>bp :bprevious<CR>
map <Leader>bs :sbprevious<CR>
map <Leader>by :YankWoleBuffer<CR>

command! Format normal gg=G''
command! YankWoleBuffer normal gg"*yG

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
map <leader>gw :StripTrailingWhitespaces<CR>

let g:lmap.l = { 'name': ' -- Layout' }
map <Leader>ls :vsplit<CR>
map <Leader>lt :TagBResise

" let g:lmap.m = { 'name': ' -- Motion(Easy)' }
" EASYMOTION MAPS
" nmap w <Plug>(easymotion-w)
" nmap W <Plug>(easymotion-W)
" nmap e <Plug>(easymotion-e)
" nmap E <Plug>(easymotion-E)
" nmap b <Plug>(easymotion-b)
" nmap B <Plug>(easymotion-B)
" nmap f <Plug>(easymotion-f)
" nmap F <Plug>(easymotion-F)
" nmap t <Plug>(easymotion-t)
" nmap T <Plug>(easymotion-T)
" map <Leader>j <Plug>(easymotion-j)
" map <Leader>k <Plug>(easymotion-k)

" let g:lmap.n = { 'name': ' -- Project' }
map <Leader>n :NERDTreeFind<CR>

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

let g:lmap.p = { 'name': ' -- Project' }
map <Leader>pg :GFiles?<CR>
map <Leader>pm :Marks<CR>
map <Leader>pn <C-W>}
map <Leader>pp :Files<CR>
map <Leader>po :only<CR>

let g:lmap.r = { 'name': 'global reg' }
map <Leader>rr "*

let g:lmap.s = { 'name': ' -- Search' }
map <leader>st :CursorInTags<CR>
map <leader>sw :FindWordUnderCursor<CR>

command! CursorInTags :call fzf#vim#tags(expand("<cword>"))<CR>

let g:lmap.t = { 'name': ' -- Tags' }
map <leader>tf :Tags<CR>
map <leader>tl :ts<CR>
map <leader>tn :tn<CR>
map <leader>tp :tp<CR>
map <leader>tt :FindTag<CR>
map <leader>th :TagbarToggle<CR>
map <leader>tw :ts "<cword>"<CR>

command! FindTag normal <C-]>

let g:lmap.u = { 'name': ' -- Utils' }
map <leader>us :sort<CR>

let g:lmap.w = { 'name': ' -- Windows' }
map <leader>wa :GoToTab1<CR>
map <leader>ws :GoToTab2<CR>
map <leader>wd :GoToTab3<CR>
map <leader>wf :GoToTab4<CR>
map <leader>wn :tabNext<CR>
map <leader>wN :tabnew<CR>
map <leader>wp :tabprevious<CR>
map <leader>wl :tabs<CR>

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
command! FindWordUnderCursor :call fzf#vim#ag(expand("<cword>"))<CR>
command! Json %!python -m json.tool
command! StripTrailingWhitespaces call <SID>StripTrailingWhitespaces()
command! SourceVimrc write | source ~/.vimrc
command! -nargs=1 TagBResise call ResiseTagBar(<f-args>)
command! DiffSaved call DiffWithSaved()

"---------------------------------------------------------------"
"--- Functions
"---------------------------------------------------------------"
" autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()
command! -bang -nargs=* Fuzzyag
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

function! ResiseTagBar(size)
        let g:tagbar_width = a:size
        exe "TagbarToggle"
        exe "TagbarOpen"
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

function! DiffWithSaved()
        let filetype=&ft
        diffthis
        vnew | r # | normal! 1Gdd
        diffthis
        exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction

"---------------------------------------------------------------"
"--- NERdTREE stuff
"---------------------------------------------------------------"
let g:NERDTreeWinSize=40 " nice big tree is it's easy to toggle off
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
"--- Retired */
"---------------------------------------------------------------" */
" let g:ctrlp_custom_ignore = { */
"                         \ 'dir': '\v[\/](\.git|tmp|node_modules|app_build|build)' */
"                         \} */
" let g:ctrlp_show_hidden = 1 */
" call ctrlp_bdelete#init() */
