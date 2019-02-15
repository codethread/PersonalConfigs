function! GenerateCtags()
    let dir = split(getcwd(),'/')[-1]
    let ctag = 'ctags -f .tags -R'
    let mapping = {
                \ 'sky-pages': ctag.' apps/{mobile,unicorn}/**/*[^test].{js,jsx} && '.ctag.' src/',
                \ 'skyport-graphql': ctag.' src/schema/',
                \ 'pages-lib': ctag.' packages/*/src/**/*[^spec].{js,jsx}',
                \ }

    let default = ctag.' .'
    exec 'AsyncRun! '. get(mapping, expand(dir), default)
    echo 'tags => '. get(mapping, expand(dir), default)
endfunction
