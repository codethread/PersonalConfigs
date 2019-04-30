let g:fzf_layout = { 'window': '5split' }
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

command! Files call fzf#run(fzf#wrap({ 
            \ 'source': 'fd -t f --hidden --follow -E ".git/" .',
            \ 'options': '--prompt '. getcwd() .'/'
            \ }))

" WIP
command! Projects call fzf#run(fzf#wrap({
            \ 'source': 'fd --type d --exclude "{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}" .',
            \ 'dir': '~',
            \ 'sink': {dir -> FileInProj(dir)},
            \ 'options': '--prompt Projects: '
            \}))

function! FileInProj(dir)
    let home = shellescape(fnamemodify('~', ':p'))
    call fzf#run(fzf#wrap({
                \ 'source': 'fd -t f --hidden -E ".git/" .',
                \ 'dir': home.a:dir.'/',
                \ 'sink': {file -> OpenFileOnly(a:dir, file)},
                \ 'options': '--prompt '. a:dir .'/'
                \}))
endfunction

function! OpenFileOnly(project, file)
    exe 'e ~/'.a:project.'/'.a:file
endfunction
