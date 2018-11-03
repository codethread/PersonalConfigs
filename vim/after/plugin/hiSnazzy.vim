let  red      =  '#ff5c57'
let  green    =  '#5af78e'
let  yellow   =  '#f3f99d'
let  blue     =  '#57c7ff'
let  magenta  =  '#ff6ac1'
let  cyan     =  '#9aedfe'
let  orange   =  '#fecc9a'
let  turqoise =  '#5af4ce'
let  light_v  =  '#d69eff'
let  coral    = '#FF776E'

hi NonText guifg=bg
hi Comment cterm=italic gui=italic

:exe 'hi ALEError cterm=underline,italic,bold guifg='.coral
hi ALEWarning cterm=underline,italic

:exe 'hi SpellBad    guifg=white guibg='.red
:exe 'hi MatchParen  guifg='.red
:exe 'hi Search  cterm=underline gui=underline guibg=bg guifg='.green
:exe 'hi Boolean guifg='.magenta
:exe 'hi Number guifg='.turqoise


:exe 'hi jsImport  guifg='.blue
:exe 'hi jsFrom  guifg='.blue
:exe 'hi jsFuncArgs  guifg='.coral.' cterm=italic'
:exe 'hi jsFuncCall  guifg='.light_v.' cterm=italic'
:exe 'hi jsThis  guifg='.coral.' cterm=bold'

:exe 'hi jsObjectKey guifg='.cyan
:exe 'hi jsObjectFuncName  guifg='.cyan

:exe 'hi jsParens  guifg='.magenta
:exe 'hi jsIfElseBraces      guifg='.magenta 
:exe 'hi jsTryCatchBraces    guifg='.magenta 
:exe 'hi jsFinallyBraces     guifg='.magenta 
:exe 'hi jsSwitchBraces      guifg='.magenta 
" :exe 'hi jsRepeatBraces      guifg='.magenta 
:exe 'hi jsDestructuringBraces guifg='.turqoise
:exe 'hi jsArrowFunction guifg='.turqoise
:exe 'hi jsClassFuncName guifg='.magenta.' cterm=underline'

:exe 'hi jsFuncBraces  guifg='.magenta
:exe 'hi jsFuncParens  guifg='.light_v
:exe 'hi jsTemplateBraces  guifg='.magenta
:exe 'hi jsObjectBraces  guifg='.turqoise
:exe 'hi jsArrayBraces  guifg='.turqoise

" :exe 'hi jsClassValue  guifg='.green

" syntax region jsxAttributeBraces
"     \ contained
"     \ start=+=\@<={+
"     \ end=+}\ze\%(\/\|\n\|\s\|>\)+
"     \ contains=TOP
"     \ keepend
"     \ extend

" jsx props
:exe 'hi Type cterm=italic guifg='.cyan
" jsx closing tag
" :exe 'hi Identifier guifg='.turqoise

:exe 'hi xmlTagName  cterm=bold guifg='.blue
:exe 'hi xmlTag cterm=bold guifg='.turqoise
" :exe 'hi xmlEndTag  guifg='.turqoise
"
:exe 'hi jsxCloseString  guifg='.blue

:exe 'hi jsSpecial  guifg='.magenta
" :exe 'hi jsxAttributeBraces  guifg='.green
" :exe 'hi htmlEndTag  guifg='.green
" :exe 'hi htmlTagName  guifg='.green
" :exe 'hi jsxAttrib guifg='.green





:exe 'hi typescriptReserved  guifg='.blue
:exe 'hi jsFrom  guifg='.blue




:exe 'hi jsonBraces  guifg='.magenta
:exe 'hi jsonKeyword  guifg='.magenta.' gui=bold cterm=bold'
:exe 'hi jsonString  guifg='.cyan
:exe 'hi jsonBoolean  guifg='.green
:exe 'hi jsonNumber  guifg='.blue
