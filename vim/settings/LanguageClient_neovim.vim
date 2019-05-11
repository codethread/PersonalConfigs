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

"" Debugging
" let g:LanguageClient_loggingLevel = 'TRACE'
" let g:LanguageClient_loggingFile =  glob('~/.local/share/nvim/LanguageClient.log')
" let g:LanguageClient_serverStderr = glob('~/.local/share/nvim/LanguageServer.log')

"" Java [gave up]
"java https://github.com/Ruin0x11/intellij-lsp-server
"java https://github.com/eclipse/eclipse.jdt.ls
" \ 'java': ['/usr/local/bin/jdtls'], life's too short to get this to work

