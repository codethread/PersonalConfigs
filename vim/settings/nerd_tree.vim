""""""""""""""""""""
"  NERDTree stuff "
""""""""""""""""""""
let g:NERDTreeWinSize=40 " nice big tree is it's easy to toggle off
let NERDTreeMinimalUI=1
let NERDTreeStatusline="%{ getcwd() }"
let NERDTreeHijackNetrw=1



let NERDTreeIgnore = ['\.DAT$', '\.LOG1$', '\.LOG1$']
let NERDTreeIgnore += [
            \ '\.swp$',
            \ '.git',
            \ 'node_modules']
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

