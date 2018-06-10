" ===============================================================
" tenderAdam
" A dark and fresh color scheme for vim
" URL: https://github/com/jacoborus/tenderAdam.vim
" Author: Jacobo Tabernero http://jacoborus.codes
" License: MIT
" Last Change: 2017/02/05 21:43
" ===============================================================

" :help highlight-default
" pack/my-packages/start/vim-javascript/syntax/javascript.vim

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name="tenderAdam"

hi ColorColumn ctermfg=NONE ctermbg=236 cterm=NONE
hi CursorColumn ctermfg=NONE ctermbg=236 cterm=NONE
hi CursorLine term=bold cterm=bold ctermbg=237
hi CursorLineNr ctermfg=81 ctermbg=NONE cterm=NONE
hi Directory ctermfg=153 ctermbg=NONE cterm=NONE
hi DiffAdd ctermfg=NONE ctermbg=238 cterm=NONE
hi DiffChange ctermfg=NONE ctermbg=239 cterm=NONE
hi DiffDelete ctermfg=203 ctermbg=237 cterm=NONE
hi DiffText ctermfg=NONE ctermbg=NONE cterm=reverse
hi ErrorMsg ctermfg=203 ctermbg=NONE cterm=reverse
hi VertSplit ctermfg=233 ctermbg=235 cterm=Bold
hi EndOfBuffer ctermfg=235 ctermbg=235
hi Folded ctermfg=242 ctermbg=234 cterm=NONE
hi FoldColumn ctermfg=242 ctermbg=234 cterm=NONE
hi IncSearch ctermfg=235 ctermbg=15 cterm=NONE
hi LineNr ctermfg=242 ctermbg=NONE cterm=NONE
hi MatchParen ctermfg=15 ctermbg=203 cterm=bold
hi NonText ctermfg=238 ctermbg=NONE cterm=NONE
hi Normal ctermfg=255 ctermbg=235 cterm=NONE
hi PMenu ctermfg=235 ctermbg=81 cterm=NONE
hi PMenuSel ctermfg=235 ctermbg=185 cterm=NONE
hi PmenuSbar ctermfg=180 ctermbg=180 cterm=NONE
hi PmenuThumb ctermfg=215 ctermbg=215 cterm=NONE
hi Question ctermfg=185 ctermbg=NONE cterm=NONE
hi Search ctermfg=10 ctermbg=NONE cterm=underline,bold
hi SpecialKey ctermfg=238 ctermbg=NONE cterm=NONE
hi SpellBad ctermfg=203 ctermbg=NONE cterm=NONE
hi SpellLocal ctermfg=180 ctermbg=NONE cterm=NONE
hi SpellCap ctermfg=215 ctermbg=NONE cterm=NONE
hi SpellRare ctermfg=81 ctermbg=NONE cterm=NONE
hi StatusLine ctermfg=234 ctermbg=242 cterm=bold
hi StatusLineNC ctermfg=246 ctermbg=238 cterm=NONE
hi TabLine ctermfg=246 ctermbg=238 cterm=NONE
hi TabLineFill ctermfg=NONE ctermbg=238 cterm=NONE
hi TabLineSel ctermfg=185 ctermbg=NONE cterm=bold
hi Title ctermfg=153 ctermbg=NONE cterm=bold
hi Visual ctermfg=NONE ctermbg=0 cterm=NONE
hi VisualNOS ctermfg=NONE ctermbg=0 cterm=NONE
hi WarningMsg ctermfg=203 ctermbg=NONE cterm=NONE
hi WildMenu ctermfg=235 ctermbg=185 cterm=bold
hi Comment ctermfg=242 ctermbg=NONE cterm=NONE
hi Constant ctermfg=215 ctermbg=NONE cterm=NONE
hi String ctermfg=180 ctermbg=NONE cterm=NONE
hi Character ctermfg=215 ctermbg=NONE cterm=NONE
hi Boolean ctermfg=215 ctermbg=NONE cterm=NONE
hi Number ctermfg=215 ctermbg=NONE cterm=NONE
hi Float ctermfg=215 ctermbg=NONE cterm=NONE
hi Identifier ctermfg=153 ctermbg=NONE cterm=NONE
hi Function ctermfg=153 ctermbg=NONE cterm=NONE
hi Statement ctermfg=153 ctermbg=NONE cterm=NONE
hi Conditional ctermfg=185 ctermbg=NONE cterm=NONE
hi Operator ctermfg=203 ctermbg=NONE cterm=NONE
hi Exception ctermfg=203 ctermbg=NONE cterm=NONE
hi PreProc ctermfg=185 ctermbg=NONE cterm=NONE
hi Type ctermfg=81 ctermbg=NONE cterm=NONE
hi Special ctermfg=81 ctermbg=NONE cterm=NONE
hi Underlined ctermfg=NONE ctermbg=NONE cterm=underline
hi Error ctermfg=255 ctermbg=203 cterm=NONE
hi Todo ctermfg=203 ctermbg=NONE cterm=bold
hi cssVendor ctermfg=142 ctermbg=NONE cterm=NONE
hi cssTagName ctermfg=142 ctermbg=NONE cterm=NONE
hi cssAttrComma ctermfg=255 ctermbg=NONE cterm=NONE
hi cssBackgroundProp ctermfg=153 ctermbg=NONE cterm=NONE
hi cssBorderProp ctermfg=153 ctermbg=NONE cterm=NONE
hi cssBoxProp ctermfg=81 ctermbg=NONE cterm=NONE
hi cssDimensionProp ctermfg=81 ctermbg=NONE cterm=NONE
hi cssFontProp ctermfg=153 ctermbg=NONE cterm=NONE
hi cssPositioningProp ctermfg=81 ctermbg=NONE cterm=NONE
hi cssTextProp ctermfg=153 ctermbg=NONE cterm=NONE
hi cssValueLength ctermfg=255 ctermbg=NONE cterm=NONE
hi cssValueInteger ctermfg=255 ctermbg=NONE cterm=NONE
hi cssValueNumber ctermfg=255 ctermbg=NONE cterm=NONE
hi cssIdentifier ctermfg=142 ctermbg=NONE cterm=NONE
hi cssIncludeKeyword ctermfg=215 ctermbg=NONE cterm=NONE
hi cssImportant ctermfg=203 ctermbg=NONE cterm=NONE
hi cssClassName ctermfg=185 ctermbg=NONE cterm=NONE
hi cssClassNameDot ctermfg=255 ctermbg=NONE cterm=NONE
hi cssProp ctermfg=153 ctermbg=NONE cterm=NONE
hi cssAttr ctermfg=255 ctermbg=NONE cterm=NONE
hi cssUnitDecorators ctermfg=255 ctermbg=NONE cterm=NONE
hi cssNoise ctermfg=203 ctermbg=NONE cterm=NONE
hi diffRemoved ctermfg=203 ctermbg=NONE cterm=NONE
hi diffChanged ctermfg=153 ctermbg=NONE cterm=NONE
hi diffAdded ctermfg=185 ctermbg=NONE cterm=NONE
hi diffSubname ctermfg=142 ctermbg=NONE cterm=NONE
hi elmDelimiter ctermfg=255 ctermbg=NONE cterm=NONE
hi elmOperator ctermfg=203 ctermbg=NONE cterm=NONE
hi FugitiveblameHash ctermfg=153 ctermbg=NONE cterm=NONE
hi FugitiveblameUncommitted ctermfg=203 ctermbg=NONE cterm=NONE
hi FugitiveblameTime ctermfg=185 ctermbg=NONE cterm=NONE
hi FugitiveblameNotCommittedYet ctermfg=215 ctermbg=NONE cterm=NONE
hi gitcommitBranch ctermfg=81 ctermbg=NONE cterm=NONE
hi gitcommitDiscardedType ctermfg=160 ctermbg=NONE cterm=NONE
hi gitcommitSelectedType ctermfg=142 ctermbg=NONE cterm=NONE
hi gitcommitHeader ctermfg=153 ctermbg=NONE cterm=NONE
hi gitcommitUntrackedFile ctermfg=215 ctermbg=NONE cterm=NONE
hi gitcommitDiscardedFile ctermfg=203 ctermbg=NONE cterm=NONE
hi gitcommitSelectedFile ctermfg=185 ctermbg=NONE cterm=NONE
hi helpHyperTextEntry ctermfg=185 ctermbg=NONE cterm=NONE
hi helpHeadline ctermfg=81 ctermbg=NONE cterm=NONE
hi helpSectionDelim ctermfg=242 ctermbg=NONE cterm=NONE
hi helpNote ctermfg=203 ctermbg=NONE cterm=NONE
hi jsonEscape ctermfg=81 ctermbg=NONE cterm=NONE
hi jsonNumber ctermfg=215 ctermbg=NONE cterm=NONE
hi jsonBraces ctermfg=255 ctermbg=NONE cterm=NONE
hi jsonNull ctermfg=215 ctermbg=NONE cterm=NONE
hi jsonBoolean ctermfg=215 ctermbg=NONE cterm=NONE
hi jsonKeywordMatch ctermfg=203 ctermbg=NONE cterm=NONE
hi jsonQuote ctermfg=255 ctermbg=NONE cterm=NONE
hi jsonNoise ctermfg=203 ctermbg=NONE cterm=NONE
hi markdownH1 ctermfg=153 ctermbg=NONE cterm=bold
hi markdownHeadingRule ctermfg=203 ctermbg=NONE cterm=bold
hi markdownHeadingDelimiter ctermfg=203 ctermbg=NONE cterm=bold
hi markdownListMarker ctermfg=215 ctermbg=NONE cterm=NONE
hi markdownBlockquote ctermfg=215 ctermbg=NONE cterm=NONE
hi markdownRule ctermfg=185 ctermbg=NONE cterm=NONE
hi markdownLinkText ctermfg=185 ctermbg=NONE cterm=NONE
hi markdownLinkTextDelimiter ctermfg=153 ctermbg=NONE cterm=NONE
hi markdownLinkDelimiter ctermfg=153 ctermbg=NONE cterm=NONE
hi markdownIdDeclaration ctermfg=142 ctermbg=NONE cterm=NONE
hi markdownAutomaticLink ctermfg=81 ctermbg=NONE cterm=NONE
hi markdownUrl ctermfg=81 ctermbg=NONE cterm=NONE
hi markdownUrlTitle ctermfg=180 ctermbg=NONE cterm=NONE
hi markdownUrlDelimiter ctermfg=215 ctermbg=NONE cterm=NONE
hi markdownUrlTitleDelimiter ctermfg=58 ctermbg=NONE cterm=NONE
hi markdownCodeDelimiter ctermfg=81 ctermbg=NONE cterm=NONE
hi markdownCode ctermfg=180 ctermbg=NONE cterm=NONE
hi markdownEscape ctermfg=81 ctermbg=NONE cterm=NONE
hi markdownError ctermfg=203 ctermbg=NONE cterm=NONE
hi NERDTreeHelp ctermfg=255 ctermbg=NONE cterm=NONE
hi NERDTreeHelpKey ctermfg=185 ctermbg=NONE cterm=NONE
hi NERDTreeHelpCommand ctermfg=215 ctermbg=NONE cterm=NONE
hi NERDTreeHelpTitle ctermfg=153 ctermbg=NONE cterm=NONE
hi NERDTreeUp ctermfg=185 ctermbg=NONE cterm=NONE
hi NERDTreeCWD ctermfg=81 ctermbg=NONE cterm=NONE
hi NERDTreeOpenable ctermfg=203 ctermbg=NONE cterm=NONE
hi NERDTreeClosable ctermfg=215 ctermbg=NONE cterm=NONE
hi pugJavascriptOutputChar ctermfg=215 ctermbg=NONE cterm=NONE
hi GitGutterAdd ctermfg=185 ctermbg=NONE cterm=NONE
hi GitGutterChange ctermfg=153 ctermbg=NONE cterm=NONE
hi GitGutterDelete ctermfg=203 ctermbg=NONE cterm=NONE
hi GitGutterChangeDelete ctermfg=203 ctermbg=NONE cterm=NONE

hi plug2 ctermfg=185 ctermbg=NONE cterm=NONE
hi plugH2 ctermfg=81 ctermbg=NONE cterm=bold
hi plugBracket ctermfg=153 ctermbg=NONE cterm=NONE
hi plugNumber ctermfg=215 ctermbg=NONE cterm=NONE
hi plugDash ctermfg=215 ctermbg=NONE cterm=NONE
hi plugStar ctermfg=180 ctermbg=NONE cterm=NONE
hi plugMessage ctermfg=215 ctermbg=NONE cterm=NONE
hi plugName ctermfg=153 ctermbg=NONE cterm=NONE
hi plugUpdate ctermfg=203 ctermbg=NONE cterm=NONE
hi plugEdge ctermfg=185 ctermbg=NONE cterm=NONE
hi plugSha ctermfg=180 ctermbg=NONE cterm=NONE
hi plugNotLoaded ctermfg=237 ctermbg=NONE cterm=NONE
hi stylusVariable ctermfg=255 ctermbg=NONE cterm=NONE
hi stylusClass ctermfg=185 ctermbg=NONE cterm=NONE
hi stylusClassChar ctermfg=153 ctermbg=NONE cterm=NONE
hi stylusId ctermfg=185 ctermbg=NONE cterm=NONE
hi stylusIdChar ctermfg=153 ctermbg=NONE cterm=NONE
hi cssVisualVal ctermfg=255 ctermbg=NONE cterm=NONE
hi stylusImport ctermfg=215 ctermbg=NONE cterm=NONE
hi vimCommentString ctermfg=58 ctermbg=NONE cterm=NONE
hi vimCommentTitle ctermfg=66 ctermbg=NONE cterm=NONE
hi vimError ctermfg=255 ctermbg=203 cterm=NONE
hi xmlNamespace ctermfg=215 ctermbg=NONE cterm=NONE
hi xmlAttribPunct ctermfg=203 ctermbg=NONE cterm=NONE
hi xmlProcessingDelim ctermfg=203 ctermbg=NONE cterm=NONE
hi yamlFlowString ctermfg=180 ctermbg=NONE cterm=NONE
hi yamlFlowStringDelimiter ctermfg=255 ctermbg=NONE cterm=NONE
hi yamlKeyValueDelimiter ctermfg=203 ctermbg=NONE cterm=NONE

hi javaScriptOpSymbols ctermfg=203 ctermbg=NONE cterm=NONE
hi javaScriptParens ctermfg=153 ctermbg=NONE cterm=NONE
hi javaScriptDocTags ctermfg=242 ctermbg=NONE cterm=NONE
hi javaScriptDocSeeTag ctermfg=66 ctermbg=NONE cterm=NONE
hi javaScriptBrowserObjects ctermfg=81 ctermbg=NONE cterm=NONE
hi javaScriptDOMObjects ctermfg=81 ctermbg=NONE cterm=NONE
hi javaScriptFuncArg ctermfg=215 ctermbg=NONE cterm=NONE
hi javaScriptOperator ctermfg=185 ctermbg=NONE cterm=NONE
hi javaScriptBraces ctermfg=153 ctermbg=NONE cterm=NONE
hi javaScriptNull ctermfg=215 ctermbg=NONE cterm=NONE

hi jsAsyncKeyword ctermfg=203 ctermbg=NONE cterm=NONE
hi jsForAwait ctermfg=203 ctermbg=NONE cterm=NONE
hi jsParensIfElse ctermfg=153 ctermbg=NONE cterm=NONE
hi jsObjectKey ctermfg=153 ctermbg=NONE cterm=NONE
hi jsObjectFuncName ctermfg=185 ctermbg=NONE cterm=NONE
hi jsArrowFunction ctermfg=142 ctermbg=NONE cterm=NONE
hi jsFunctionKey ctermfg=185 ctermbg=NONE cterm=NONE
hi jsFuncName ctermfg=185 ctermbg=NONE cterm=NONE
hi jsFuncParens ctermfg=153 ctermbg=NONE cterm=NONE
hi jsFuncArgs ctermfg=215 ctermbg=NONE cterm=NONE
hi jsFuncCall  ctermfg=081 ctermbg=NONE cterm=bold
hi jsReturn ctermfg=203 ctermbg=NONE cterm=NONE
hi jsNull ctermfg=215 ctermbg=NONE cterm=NONE
hi jsObjectColon ctermfg=203 ctermbg=NONE cterm=NONE
hi jsSpecial ctermfg=215 ctermbg=NONE cterm=NONE
hi jsTemplateBraces ctermfg=203 ctermbg=NONE cterm=NONE
hi jsGlobalObjects ctermfg=81 ctermbg=NONE cterm=NONE
hi jsGlobalNodeObjects ctermfg=153 ctermbg=NONE cterm=NONE
hi jsImport ctermfg=81 ctermbg=NONE cterm=NONE
hi jsExport ctermfg=81 ctermbg=NONE cterm=NONE
hi jsExportDefault ctermfg=185 ctermbg=NONE cterm=NONE
hi jsExportDefaultGroup ctermfg=81 ctermbg=NONE cterm=NONE
hi jsFrom ctermfg=81 ctermbg=NONE cterm=NONE
hi jsParen ctermfg=153 ctermbg=NONE cterm=italic
hi jsParenDecorator ctermfg=203 ctermbg=NONE cterm=NONE
hi jsParenIfElse   ctermfg=215 ctermbg=NONE cterm=NONE
hi jsParenRepeat   ctermfg=215 ctermbg=NONE cterm=NONE
hi jsParenSwitch   ctermfg=215 ctermbg=NONE cterm=NONE
hi jsParenCatch    ctermfg=215 ctermbg=NONE cterm=NONE
hi jsClassDefinition ctermfg=185 ctermbg=NONE cterm=bold

" syntax match bracks contained /[{}]/
hi jsBracket ctermfg=185 ctermbg=NONE cterm=NONE

let g:terminal_color_foreground = "#282828"
let g:terminal_color_background = "#eeeeee"
let g:terminal_color_0 = "#282828"
let g:terminal_color_1 = "#f43753"
let g:terminal_color_2 = "#c9d05c"
let g:terminal_color_3 = "#ffc24b"
let g:terminal_color_4 = "#b3deef"
let g:terminal_color_5 = "#d3b987"
let g:terminal_color_6 = "#73cef4"
let g:terminal_color_7 = "#eeeeee"
let g:terminal_color_8 = "#1d1d1d"
let g:terminal_color_9 = "#f43753"
let g:terminal_color_10 = "#c9d05c"
let g:terminal_color_11 = "#ffc24b"
let g:terminal_color_12 = "#b3deef"
let g:terminal_color_13 = "#d3b987"
let g:terminal_color_14 = "#73cef4"
let g:terminal_color_15 = "#ffffff"



" ===================================
" Generated by Estilo 1.3.3
" https://github.com/jacoborus/estilo
" ===================================
