runtime colors/snazzy.vim

set notermguicolors

let  red         = '#ff5c57'
let  c_red       = '001'
let  green       = '#5af78e'
let  c_green     = '002'
let  yellow      = '#f3f99d'
let  c_yellow    = '003'
let  blue        = '#57c7ff'
let  c_blue      = '004'
let  magenta     = '#ff6ac1'
let  c_magenta   = '005'
let  cyan        = '#9aedfe'
let  c_cyan      = '006'
let  orange      = '#fecc9a'
let  c_orange    = '222'
let  turqoise    = '#5af4ce'
let  c_turqoise  = '086'
let  light_v     = '#d69eff'
let  c_light_v   = '183'
let  coral       = '#FF776E'
let  c_coral     = '209'
let  c_bg_bright = '000'
let  ui_0        = '#F9F9F9'
let  ui_1        = '#f9f9ff'
let  ui_2        = '#eff0eb'
let  ui_3        = '#e2e4e5'
let  ui_4        = '#a1a6a8'
let  c_ui_4      = '248'
let  ui_5        = '#848688'
let  ui_6        = '#5e6c70'
let  ui_7        = '#536991'
let  ui_8        = '#606580'
let  c_ui_8      = '60'
let  ui_9        = '#3a3d4d'
let  c_ui_9      = '236' " bit off
let  ui_11       = '#282a36'

" Editor
" highlight SignColumn ctermbg=
let g:gitgutter_override_sign_column_highlight = 0

hi Normal                guibg=NONE  ctermbg=NONE
hi SignColumn            guibg=bg    ctermbg=NONE
hi GitGutterAdd          ctermbg=NONE ctermfg=002
hi GitGutterChange       ctermbg=NONE ctermfg=003
hi GitGutterDelete       ctermbg=NONE ctermfg=001
hi GitGutterChangeDelete ctermbg=NONE ctermfg=001
hi StatusLineNC          ctermbg=000
hi StatusLine            ctermbg=000


" netrw
:exe 'hi netrwDir       guibg=bg ctermbg=NONE ctermfg=003'
:exe 'hi NERDTreeDir    guibg=bg ctermbg=NONE ctermfg='.c_light_v
:exe 'hi CursorLine     ctermbg=000'
:exe 'hi Directory      guibg=bg cterm=bold ctermfg='.c_magenta.' ctermbg=NONE'
:exe 'hi NonText        guibg=bg ctermbg=NONE ctermfg=NONE'
:exe 'hi Comment        cterm=italic gui=italic'
:exe 'hi SpellBad       guifg=white guibg='.red.' ctermfg=255 ctermbg='.c_red
:exe 'hi MatchParen     guifg='.red.' ctermfg='.c_red
hi Search ctermfg=NONE ctermbg=000
hi Visual ctermfg=NONE ctermbg=000
:exe 'hi Boolean        guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi Number         guifg='.turqoise

:exe 'hi Folded         guifg='.cyan.' guibg='.ui_9.' ctermfg='.c_cyan.' ctermbg='.c_bg_bright
:exe 'hi VertSplit ctermfg=000 ctermbg=NONE'

" :exe 'hi TabLineFill    guibg='.ui_9.' ctermbg=NONE'
" :exe 'hi TabLineSel     guifg='.cyan.' guibg='.ui_9. ' ctermfg='.c_magenta.' ctermbg=NONE cterm=underline'
" :exe 'hi TabLine        guifg='.ui_4.' guibg='.ui_9.' ctermfg='.c_ui_4.' ctermbg=NONE cterm=underline'

" ALE
:exe 'hi ALEError       cterm=underline,italic,bold guifg='.coral.' ctermfg='.c_coral
:exe 'hi ALEErrorSign   guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi ALEWarningSign guifg='.magenta.' ctermfg='.c_magenta
hi ALEWarning           cterm=underline,italic

" json
:exe 'hi jsonBraces     guifg='.magenta.'   ctermfg='.c_magenta
:exe 'hi jsonKeyword    guifg='.magenta.'   ctermfg='.c_magenta.' gui=bold cterm=bold'
:exe 'hi jsonString     guifg='.cyan.'      ctermfg='.c_cyan
:exe 'hi jsonBoolean    guifg='.green.'     ctermfg='.c_green
:exe 'hi jsonNumber     guifg='.blue.'      ctermfg='.c_blue

" graphql
:exe 'hi graphqlString      guifg='.ui_8.'    ctermfg='.c_ui_8
:exe 'hi graphqlType        guifg='.light_v.' ctermfg='.c_light_v
:exe 'hi graphqlStructure   guifg='.magenta.' ctermfg='.c_magenta.' cterm=underline'
" :exe 'hi graphqlName guifg='.light_v


" Javascript
:exe 'hi jsImport       guifg='.blue.' ctermfg='.c_blue
:exe 'hi jsFrom         guifg='.blue.' ctermfg='.c_blue
:exe 'hi jsFuncArgs     guifg='.coral.' ctermfg='.c_coral.' cterm=italic'
:exe 'hi jsFuncCall     guifg='.light_v.' ctermfg='.c_light_v.' cterm=italic'
:exe 'hi jsThis         guifg='.coral.' ctermfg='.c_coral.' cterm=bold'
:exe 'hi jsStorageClass guifg='.blue.' ctermfg='.c_blue.' cterm=bold'

:exe 'hi jsObjectKey        guifg='.cyan.' ctermfg='.c_cyan
:exe 'hi jsObjectFuncName   guifg='.cyan.' ctermfg='.c_cyan

:exe 'hi jsParens           guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi jsIfElseBraces     guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi jsTryCatchBraces   guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi jsFinallyBraces    guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi jsSwitchBraces     guifg='.magenta.' ctermfg='.c_magenta
" :exe 'hi jsRepeatBraces      guifg='.magenta 
:exe 'hi jsDestructuringBraces      guifg='.turqoise.' ctermfg='.c_turqoise
:exe 'hi jsDestructuringProperty    guifg='.coral.' ctermfg='.c_coral
:exe 'hi jsDestructuringBlock       guifg='.coral.' ctermfg='.c_coral

:exe 'hi jsArrowFunction guifg='.turqoise.' ctermfg='.c_turqoise
:exe 'hi jsClassFuncName guifg='.magenta.' ctermfg='.c_magenta.' cterm=underline'

:exe 'hi jsFuncBraces       guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi jsFuncParens       guifg='.light_v.' ctermfg='.c_light_v
:exe 'hi jsTemplateBraces   guifg='.magenta.' ctermfg='.c_magenta
:exe 'hi jsObjectBraces     guifg='.turqoise.' ctermfg='.c_turqoise
:exe 'hi jsArrayBraces      guifg='.turqoise.' ctermfg='.c_turqoise

" jsx
:exe 'hi Type ctermfg='.c_cyan
:exe 'hi xmlTagName  cterm=bold guifg='.blue.' ctermfg='.c_blue
:exe 'hi xmlTag      cterm=bold guifg='.turqoise.' ctermfg='.c_turqoise
:exe 'hi xmlEndTag   guifg='.turqoise.' ctermfg='.c_turqoise
:exe 'hi jsxCloseString  guifg='.blue.' ctermfg='.c_blue
:exe 'hi jsSpecial  guifg='.magenta.' ctermfg='.c_blue

" CSS only for non gui
:exe 'hi cssBoxProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssFontProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssTextProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssAnimationProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssMediaProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssBackgroundProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssPositioningProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssFlexibleBoxProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssBorderProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssTransformProp      ctermbg=NONE ctermfg=white'
:exe 'hi cssTransitionProp      ctermbg=NONE ctermfg=white'

:exe 'hi cssBoxAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssFontAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssTextAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssAnimationAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssMediaAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssBackgroundAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssPositioningAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssFlexibleBoxAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssBorderAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssTransformAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssTransitionAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssMultiColumnAttr      ctermbg=NONE ctermfg=magenta'

:exe 'hi cssFontAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssAuralAttr      ctermbg=NONE ctermfg=magenta'
:exe 'hi cssAttrComma      ctermbg=NONE ctermfg=cyan'

:exe 'hi cssColor      ctermbg=NONE ctermfg=blue'
:exe 'hi cssClassName      ctermbg=NONE ctermfg=cyan'
:exe 'hi cssClassNameDot      ctermbg=NONE ctermfg=cyan'

:exe 'hi cssValueNumber  ctermbg=NONE ctermfg=green'
:exe 'hi cssValueTime  ctermbg=NONE ctermfg=green'
:exe 'hi cssValueLength  ctermbg=NONE ctermfg=green'

" typescript
:exe 'hi typescriptEndColons ctermfg=fg'
:exe 'hi typescriptStorageClass cterm=bold ctermfg='.c_cyan
:exe 'hi typescriptReserved cterm=italic ctermfg='.c_blue
:exe 'hi typescriptLabel cterm=bold ctermfg='.c_magenta
:exe 'hi typescriptBranch cterm=bold ctermfg='.c_red

" yats XXX too slow
:exe 'hi typescriptExport ctermfg='.c_magenta
:exe 'hi typescriptImport ctermfg='.c_blue
:exe 'hi typescriptVariable  cterm=bold ctermfg='c_blue
:exe 'hi typescriptObjectLabel ctermfg='.c_cyan
:exe 'hi typescriptArrowFunc ctermfg='.c_turqoise
:exe 'hi typescriptBraces ctermfg='.c_turqoise
:exe 'hi typescriptExceptions cterm=bold ctermfg='.c_red
:exe 'hi typescriptTry cterm=bold ctermfg='.c_red
:exe 'hi typescriptTemplateSB ctermfg='.c_magenta
:exe 'hi typescriptGlobalMethod ctermfg='.c_light_v
:exe 'hi typescriptTypeReference ctermfg='.c_turqoise

" startify just highlights the panda white
highlight StartifyHeader  ctermfg=white
