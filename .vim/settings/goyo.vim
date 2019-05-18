"""""""""""""""""""" */
"  GOYO "
"""""""""""""""""""" */
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
