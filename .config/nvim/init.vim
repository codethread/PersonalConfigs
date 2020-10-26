"                %%%%(         /%%%%               
"            %%#                     (%%           
"         %%                             %%        
"       %%                                 %%      
"     .%           %            %%%%         %     
"    (%           %%%           %%%%          %/   
"    %           %%%%           %%%%           %   
"   %%          %%%%       %%%%%%%%%           %%  
"   %%         %%%%         %%%%%%%%           %%  
"   %%        %%%%%%%%%%%       %%%%           %%  
"    %       %%%%%%%%%%%%%      %%%%           %   
"    %%     %%%%*               %%%%          %%   
"     %%   %%%%(                %%%%         %#    
"       %%                                 %%      
"         %%                             %%        
"           *%%                       %%,          
"               (%%%/           /%%%/  
"
call plug#begin('~/.vim/plugged')

Plug 'editorconfig/editorconfig-vim'
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
" codi.vim {{{
Plug 'metakirby5/codi.vim', { 'on': 'Codi' }
let g:codi#rightsplit = 0
let g:codi#rightalign = 0
let g:codi#width = 80

" }}}
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'tpope/vim-abolish' " coerce words such as crs: coerce to snake_case
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-abolish' " coerce words such as crs: coerce to snake_case
Plug 'tpope/vim-repeat'
Plug 'wakatime/vim-wakatime'
Plug 'airblade/vim-gitgutter'
Plug 'solarnz/thrift.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'godlygeek/tabular'
Plug 'arcticicestudio/nord-vim'
let g:nord_italic = 1
let g:nord_italic_comments = 1
let g:nord_underline = 1
Plug 'joshdick/onedark.vim'
let g:onedark_terminal_italics = 1
let g:one_allow_italics = 1 " I love italic for comments
" lightline.vim  {{{
Plug 'itchyny/lightline.vim'
function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'cocstatus', 'currentfunction', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \     'cocstatus': 'coc#status',
      \     'currentfunction': 'CocCurrentFunction'
      \   },
      \ }
" }}}
" Plug 'rakr/vim-one'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
" vim-projectionist {{{
Plug 'tpope/vim-projectionist'
let g:projectionist_heuristics = {
      \   "*.jsx": {
      \     "alternate": "{}.spec.js"
      \   },
      \   "*.spec.js": {
      \     "alternate": "{}.jsx"
      \   }
      \ }
" }}}
Plug 'neoclide/coc.nvim', {'branch': 'release'}
let g:coc_global_extensions = [
      \ 'coc-tsserver',
      \ 'coc-css',
      \ 'coc-eslint',
      \ 'coc-html',
      \ 'coc-json',
      \ 'coc-lists',
      \ 'coc-metals',
      \ 'coc-rls',
      \ 'coc-marketplace',
      \ 'coc-vimlsp',
      \ 'coc-yaml',
      \ 'coc-sh',
      \ 'coc-docker',
      \ ]
Plug 'easymotion/vim-easymotion'
let g:EasyMotion_do_mapping = 0 " turn off default mapping
let g:EasyMotion_smartcase = 1 " Turn on case-insensitive feature
" nmap <silent> s <Plug>(easymotion-prefix)
nmap <silent>s <Plug>(easymotion-s)
nmap <silent>S <Plug>(easymotion-jumptoanywhere)

" disable coc briefly while easymotion changes the buffer
let g:easymotion#is_active = 0
function! EasyMotionCoc() abort
  if EasyMotion#is_active()
    let g:easymotion#is_active = 1
    CocDisable
  else
    if g:easymotion#is_active == 1
      let g:easymotion#is_active = 0
      CocEnable
    endif
  endif
endfunction
autocmd TextChanged,CursorMoved * call EasyMotionCoc()


" overwrite the default as I usually prefer to find the begining of a word
let g:EasyMotion_re_anywhere = '\v' . '(<.|^$)'
let g:EasyMotion_keys = 'ASDF:LKJqweruiopgha;sldkfj'

" Initialize plugin system
call plug#end()

if has('nvim') || has('termguicolors')
  set termguicolors
endif

" colorscheme onedark
colorscheme nord
" highlight Normal ctermbg=none
" highlight NonText ctermbg=none
nnoremap Q :echo "Q (Ex mode) has been disabled in vimrc because it's a little shit"<CR>

let mapleader = " "
set nobackup
set grepprg=rg\ --hidden\ --glob\ '!.git'\ --vimgrep\ --with-filename
set nowritebackup
" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c
" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

set clipboard+=unnamedplus
set autowrite        " write if modified, such as when running :make
set expandtab        " tabs are spaces
set hidden           " allows hiding modified buffers
set shiftwidth=2     " number of spaces in tab when editing
set showtabline=2    " Show tabline
set smartcase        " search ignores case unless capitals present
set sps=best,10      " spell only shows top 10 results
set tabstop=2        " number of visual spaces per TAB
set inccommand=split " live find and replace

" let g:fzf_layout = { 'window': 'botright 9split' }

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

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
" set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
" nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
" nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
" nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
" nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
" nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
" nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

command! SourceVimrc write | so $MYVIMRC
map <leader>gs :SourceVimrc<CR>
map <leader>gv :e $MYVIMRC<CR>

" p - Project {{{
map <leader>pp :Projects<CR>
" }}}

" map <leader><SPACE> :Files<CR>
map <leader><SPACE> :<C-u>CocList files<cr>

" error
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <leader>ep <Plug>(coc-diagnostic-prev)
nmap <leader>en <Plug>(coc-diagnostic-next)

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

  let s:whitespace = repeat(' ', 40 - strwidth(s:line) - len(s:info))
  let s:message = s:symbol . s:line . s:whitespace .s:info
  return s:message . repeat(' ', winwidth(winnr('$')) - len(s:message))
endfunction

augroup my_filetypes
  autocmd!
  autocmd BufNewFile,BufRead *.{md,mdwn,mkd,mkdn,mark*} set filetype=markdown
  autocmd BufRead .vimrc set foldmethod=marker | set foldlevel=1 | set foldtext=VimFoldText()
augroup END

map <leader>bN :enew<CR>
map <leader>bb :SplitPreviousBuffer<CR>
command! SplitPreviousBuffer :vsplit | bprevious
map <leader>bc :BufOnly<CR>
map <leader>bd :DiffSaved<CR>
command! DiffSaved call DiffWithSaved()
map <leader>bk :Bkill!<CR>
map <leader>bl :<C-u>CocList buffers<cr>
map <leader>bn :bnext<CR>
map <leader>bp :bprevious<CR>
command! DeleteFileAndBuff :call delete(expand('%')) | bd
map <leader>bt :b#<CR>
map <leader>by :YankWoleBuffer<CR>
command! YankWoleBuffer normal gg"*yG

function! DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction

map <leader>ff :Files<CR>
map <leader>fr :Rename<space>
map <leader>fq :DeleteFileAndBuff<CR>
map <leader>fs :<C-u>CocList words<cr>

" w - Windows / Panes {{{
" let g:lmap.w = { 'name': ' -- Windows' }
map <leader>wr :redraw!<CR>
map <leader>wN :tabnew<CR>
map <leader>wl :tabs<CR>
map <leader>wn :tabNext<CR>
map <leader>wp :tabprevious<CR>
map <leader>wk :close<CR>
" map <leader>wo :Goyo<CR>
" map <leader>wp :pclose<CR>
map <leader>wT :TagBResise
command! -nargs=1 TagBResise call ResiseTagBar(<f-args>)
map <leader>wt :TagbarToggle<CR>
map <leader>ww :vsplit<CR>
" }}}
map <leader>ps :<C-u>CocList grep<cr>
