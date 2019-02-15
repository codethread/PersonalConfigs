function! HandleJsConfig()
    try
        let jsconfig = ParseJSON(system('cat ' . getcwd() . '/jsconfig.json'))
        let baseUrl = jsconfig.compilerOptions.baseUrl
        let paths = jsconfig.compilerOptions.paths
        return [baseUrl, paths]
    catch
        echo 'no jsconfig.json'
    endtry
endfunction

function! SearchForDefinition(name, callCount)
    " try tags first
    " try | exec 'tag ' . a:object | return | catch | silent | endtry
    let object = a:name
    let isMethod = 0
    if a:callCount == 1
        let isMethod = substitute(expand("<cWORD>"), '\([a-zA-Z\.]\+\)(*.*', '\1', "g") =~ '\.'
        if isMethod
            normal B
            let object = expand("<cword>")
        endif
    endif

    let quotes = "['" . '"]'
    let langspecific = "\\(from \\|require(\\)"
    let patt = '\<' . object . '\>' . '\_[^;]\{-}' . langspecific . quotes . '.\+' . quotes . "[\s;)]*\$"

    if search(patt, 'b') == 0 " look for import
        call search('\(' . object . '\|export default\)') " look for export
        return
    endif

    call search(quotes, 'e') " go to file name
    " find out if relative or global
    " js specific index file check
    let workdir = getcwd()

    let isRel = nr2char(strgetchar(getline('.'), col('.'))) =~ '\.'
    if isRel
        exec 'cd '. expand('%:p:h')
        let pathUnderCursor = expand("<cfile>")
    else
        " if needs be can do more with paths, for now just use baseurl
        let [baseUrl, paths] = HandleJsConfig()
        let rootPath = substitute(baseUrl, '\(./\|.\)', "", "")
        let pathUnderCursor = rootPath . expand("<cfile>")
    endif

    let files = getcompletion(pathUnderCursor . '.', 'file')
    if len(files) >= 1
        try
            if len(files) == 0
                let [file] = getcompletion(pathUnderCursor, 'file')
            else
                let [file; rest] = filter(files, 'v:val =~ "\\(\.js\\|\.jsx\\)"')
            endif
        catch
            " not supporting node modules for now
            echo 'node module'
            exec 'cd '. workdir
            return
        endtry
    else
        try 
            let [file; rest] = getcompletion(pathUnderCursor . '/index', 'file')
        catch
            echo 'no index?'
        endtry
    endif

    if &mod " modified so open split
        exec 'vsplit ' . file
    else
        exec 'e ' . file
    endif

    " put the cd back in order
    exec 'cd '. workdir

    if isMethod
        call SearchForDefinition(a:name, (a:callCount + 1))
    else
        call SearchForDefinition(object, (a:callCount + 1))
    endif
endfunction

