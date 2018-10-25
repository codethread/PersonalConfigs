function! SearchForDefinition(name)
    " try tags first
    " try | exec 'tag ' . a:name | return | catch | silent | endtry

    let quotes = "['" . '"]'
    let langspecific = "\\(from \\|require(\\)"
    let patt = '\<' . a:name . '\>' . '\_[^;]\{-}' . langspecific . quotes . '.\+' . quotes . "[\s;)]*\$"

    if search(patt, 'b') == 0 " look for import
        call search('\(' . a:name . '\|export default\)') " look for export
        return
    endif

    call search(quotes, 'e') " go to file name
    " find out if relative or global
    " js specific index file check
    let workdir = getcwd()


    let isRel = nr2char(strgetchar(getline('.'), col('.'))) =~ '\.'
    if isRel
        exec 'cd '. expand('%:p:h')
    endif

    let options = globpath(expand("<cfile>"), '*',0,1)
    if len(options) == 0 " length = 0 path
        try
            let [file] = getcompletion(expand("<cfile>") . '.', 'file')
        catch
            echo 'node module'
            exec 'cd '. workdir
            return
        endtry
    else
        let [file] = getcompletion(expand("<cfile>") . '/index', 'file')
    endif

    if &mod " modified so open split
        exec 'vsplit ' . file
    else
        exec 'e ' . file
    endif

    " put the cd back in order
    exec 'cd '. workdir

    " recurse
    call SearchForDefinition(a:name)
endfunction
