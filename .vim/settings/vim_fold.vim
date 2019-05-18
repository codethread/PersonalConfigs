""""""""""""""""""""""""
"  THIS IS A CATEGORY  "
""""""""""""""""""""""""
" https://vi.stackexchange.com/questions/3814/is-there-a-best-practice-to-fold-a-vimrc-file
"" Autofolding .vimrc
" see http://vimcasts.org/episodes/writing-a-custom-fold-expression/
""" defines a foldlevel for each line of code

function! VimFolds(lnum)
    let s:thisline = getline(a:lnum)
    if match(s:thisline, '^"" ') >= 0
        return '>2'
    endif
    if match(s:thisline, '^""" ') >= 0
        return '>3'
    endif
    let s:two_following_lines = 0
    if line(a:lnum) + 2 <= line('$')
        let s:line_1_after = getline(a:lnum+1)
        let s:line_2_after = getline(a:lnum+2)
        let s:two_following_lines = 1
    endif
    if !s:two_following_lines
        return '='
    endif
else
    if (match(s:thisline, '^"""""') >= 0) &&
                \ (match(s:line_1_after, '^"  ') >= 0) &&
                \ (match(s:line_2_after, '^""""') >= 0)
        return '>1'
    else
        return '='
    endif
endif
endfunction

""" defines a foldtext
function! VimFoldText()
    " handle special case of normal comment first
    let s:info = '('.string(v:foldend-v:foldstart).' l)'
    if v:foldlevel == 1
        let s:line = ' ● '.getline(v:foldstart+1)[3:-2]
    elseif v:foldlevel == 2
        let s:line = '   ◇  '.getline(v:foldstart)[3:]
    elseif v:foldlevel == 3
        let s:line = '     ▪ '.getline(v:foldstart)[4:]
    endif
    if strwidth(s:line) > 80 - len(s:info) - 3
        return s:line[:79-len(s:info)-3+len(s:line)-strwidth(s:line)].'...'.s:info
    else
        return s:line.repeat(' ', 80 - strwidth(s:line) - len(s:info)).s:info
    endif
endfunction

""" set foldsettings automatically for vim files
augroup fold_vimrc
    autocmd!
    autocmd FileType vim 
                \ setlocal foldmethod=expr |
                \ setlocal foldexpr=VimFolds(v:lnum) |
                \ setlocal foldtext=VimFoldText() |
                " \ set foldcolumn=2 foldminlines=2
augroup END

