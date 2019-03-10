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

let  ui_0     =  '#F9F9F9'
let  ui_1     =  '#f9f9ff'
let  ui_2     =  '#eff0eb'
let  ui_3     =  '#e2e4e5'
let  ui_4     =  '#a1a6a8'
let  ui_5     =  '#848688'
let  ui_6     =  '#5e6c70'
let  ui_7     =  '#536991'
let  ui_8     =  '#606580'
let  ui_9     =  '#3a3d4d'
let  ui_11    =  '#282a36'
let  ui_12    =  '#192224'

hi NonText guifg=bg
hi Comment cterm=italic gui=italic

:exe 'hi Folded  guifg='.cyan.' guibg='.ui_9
" :exe 'hi FoldColumn  guifg='.cyan.' guibg='.ui_9

:exe 'hi TabLineFill  guibg='.ui_9
:exe 'hi TabLine  guifg='.ui_4.' guibg='.ui_9
:exe 'hi TabLineSel  guifg='.cyan.' guibg='.ui_9

:exe 'hi ALEError cterm=underline,italic,bold guifg='.coral
" :exe 'hi ALEErrorSign guifg='.coral
:exe 'hi ALEErrorSign guifg='.magenta
:exe 'hi ALEWarningSign guifg='.magenta

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

" GRAPHQL
:exe 'hi graphqlString  guifg='.ui_8
:exe 'hi graphqlType guifg='.light_v
:exe 'hi graphqlStructure guifg='.magenta.' cterm=underline'
" :exe 'hi graphqlName guifg='.light_v
"

