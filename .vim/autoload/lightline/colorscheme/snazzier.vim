" =============================================================================
" Filename: autoload/lightline/colorscheme/snazzier.vim
" =============================================================================

let s:base03 = [ '#282a36', 233 ]
let s:base02 = [ '#3a3d4d', 008 ]
let s:base01 = [ '#44485b', 239 ]
let s:base00 = [ '#626784', 242  ]
let s:base0 = [ '#858db5', 244 ]
let s:base1 = [ '#949ecc', 246 ]
let s:base2 = [ '#a8b4ea', 248 ]
let s:base3 = [ '#f1f1f0', 253 ]
let s:yellow = [ '#f3f99d', 003 ]
let s:orange = [ '#fad07a', 222 ]
let s:red = [ '#ff5c57', 001 ]
let s:magenta = [ '#ff6ac1', 005 ]
let s:blue = [ '#57c7ff', 004 ]
let s:cyan = [ '#9aedfe', 006 ]
let s:green = [ '#5af78e', 002 ]

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}, 'command': {}}
let s:p.normal.left = [ [ s:base02, s:blue ], [ s:base3, s:base01 ] ]
let s:p.normal.right = [ [ s:base02, s:base1 ], [ s:base2, s:base01 ] ]
let s:p.inactive.right = [ [ s:base02, s:base00 ], [ s:base0, s:base02 ] ]
let s:p.inactive.left =  [ [ s:base0, s:base02 ], [ s:base00, s:base02 ] ]
let s:p.insert.left = [ [ s:base02, s:green ], [ s:base3, s:base01 ] ]
let s:p.replace.left = [ [ s:base02, s:red ], [ s:base3, s:base01 ] ]
let s:p.visual.left = [ [ s:base02, s:magenta ], [ s:base3, s:base01 ] ]
let s:p.normal.middle = [ [ s:base0, s:base02 ] ]
let s:p.inactive.middle = [ [ s:base00, s:base02 ] ]
let s:p.tabline.left = [ [ s:base3, s:base00 ] ]
let s:p.tabline.tabsel = [ [ s:base3, s:base02 ] ]
let s:p.tabline.middle = [ [ s:base01, s:base1 ] ]
let s:p.tabline.right = copy(s:p.normal.right)
let s:p.command.left = [ [ s:base02, s:orange ], [ s:base3, s:base01 ] ]
let s:p.normal.error = [ [ s:red, s:base02 ] ]
let s:p.normal.warning = [ [ s:yellow, s:base01 ] ]

let g:lightline#colorscheme#snazzier#palette = lightline#colorscheme#flatten(s:p)
