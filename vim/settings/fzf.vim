""""""""""""""""""""
"  FZF "
""""""""""""""""""""
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

command! -bang -nargs=* Fuzzyag
            \ call fzf#vim#ag(<q-args>,
            \     <bang>0 ? fzf#vim#with_preview('up:60%')
            \             : fzf#vim#with_preview('right:50%:hidden', '?'),
            \     <bang>0)

" Similarly, we can apply it to fzf#vim#grep. To use ripgrep instead of ag:
" command! -bang -nargs=* Fuzzyag
"             \ call fzf#vim#grep(
"             \   'rg --column --line-number --no-heading --color=always --smart-case'.shellescape(<q-args>), 1,
"             \   <bang>0 ? fzf#vim#with_preview('up:60%')
"             \           : fzf#vim#with_preview('right:50%:hidden', '?'),
"             \   <bang>0)

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



