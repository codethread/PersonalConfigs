" Additional syntax highlighting for specific patterns in markdown
" This works alongside Tree-sitter to highlight specific text patterns
syntax clear

" Match cc file references @foo/bar style references
syntax match markdownAtReference /@\S\+/ containedin=ALL


" Match cc thinking keywords (case-sensitive, whole words)
syntax match markdownThink /\<think\>/ containedin=ALL
syntax match markdownThinkHard /\<think hard\>/ containedin=ALL
syntax match markdownThinkHarder /\<think harder\>/ containedin=ALL
syntax match markdownUltrathink /\<ultrathink\>/ containedin=ALL

syntax match markdownImportant /\<IMPORTANT\>/ containedin=ALL
syntax match markdownMUST /\<YOU MUST\>/ containedin=ALL

" Link to highlight groups
highlight link markdownAtReference @keyword.return
highlight link markdownThink WarningMsg
highlight link markdownThinkHard WarningMsg
highlight link markdownThinkHarder Error
highlight link markdownImportant WarningMsg
highlight link markdownMUST WarningMsg

" Custom highlight for ultrathink with italic
highlight markdownUltrathink gui=italic cterm=italic guifg=#ff5555 ctermfg=Red
