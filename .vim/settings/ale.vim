""""""""""""""""""""
"  ALE linting     "
""""""""""""""""""""
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

