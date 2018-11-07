function! GenerateCtags()
    let dir = split(getcwd(),'/')[-1]
    let mapping = {
                \ "sky-pages": "ctags -f .tags -R apps/{mobile,unicorn}/**/*[^test].{js,jsx} && ctags -f .tags -R -a src/",
                \ "skyport-graphql": "ctags -f .tags -R src/schema/",
                \ "pages-lib": "ctags -f .tags -R packages/*/src/**/*[^spec].{js,jsx}",
                \ }

    let default = "ctags -f .tags -R ."
    exec 'AsyncRun! '. get(mapping, expand(dir), default)
    echo ''
endfunction
