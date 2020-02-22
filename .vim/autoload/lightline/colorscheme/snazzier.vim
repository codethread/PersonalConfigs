" =============================================================================
" Filename: autoload/lightline/colorscheme/snazzier.vim
" =============================================================================

let s:snazzBlack = [ '#3a3d4d', 000 ]
let s:bg = [ '#626784', 'NONE'  ]
let s:base0 = [ '#858db5', 244 ]
let s:base1 = [ '#949ecc', 246 ]
let s:base2 = [ '#a8b4ea', 248 ]
let s:white = [ '#f1f1f0', 253 ]
let s:yellow = [ '#f3f99d', 003 ]
let s:orange = [ '#fad07a', 222 ]
let s:red = [ '#ff5c57', 001 ]
let s:magenta = [ '#ff6ac1', 005 ]
let s:blue = [ '#57c7ff', 004 ]
let s:cyan = [ '#9aedfe', 006 ]
let s:green = [ '#5af78e', 002 ]
let s:coral = [ '#FF776E',  '209']

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}, 'command': {}}
let s:p.normal.right = [ [ s:white, s:snazzBlack ], [ s:white, s:snazzBlack ] ]
let s:p.normal.middle = [ [ s:base0, s:bg ] ]

let s:p.normal.left = [ [ s:snazzBlack, s:blue ], [ s:white, s:snazzBlack ] ]
let s:p.insert.left = [ [ s:snazzBlack, s:green ], [ s:white, s:snazzBlack ] ]
let s:p.replace.left = [ [ s:snazzBlack, s:red ], [ s:white, s:snazzBlack ] ]
let s:p.visual.left = [ [ s:snazzBlack, s:magenta ], [ s:white, s:snazzBlack ] ]

let s:p.inactive.left =  [ [ s:base0, s:snazzBlack ], [ s:bg, s:snazzBlack ] ]
let s:p.inactive.right = [ [ s:base1, s:snazzBlack ], [ s:base1, s:snazzBlack ] ]
let s:p.inactive.middle = [ [ s:base0, s:bg ] ]

let s:p.tabline.left = [ [ s:cyan, s:snazzBlack ] ]
let s:p.tabline.tabsel = [ [ s:magenta, s:snazzBlack ] ]
let s:p.tabline.middle = [ [ s:bg, s:bg ] ]

let s:p.tabline.right = [ [ s:coral, s:snazzBlack ] ]

let s:p.command.left = [ [ s:snazzBlack, s:orange ], [ s:white, s:snazzBlack ] ]
let s:p.normal.error = [ [ s:red, s:snazzBlack ] ]
let s:p.normal.warning = [ [ s:yellow, s:snazzBlack ] ]

let g:lightline#colorscheme#snazzier#palette = lightline#colorscheme#flatten(s:p)
