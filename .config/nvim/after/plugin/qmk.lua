local require = require('codethread.utils').require
local qmk, ok = require 'qmk'
if not ok then return end

qmk.setup {
	name = 'LAYOUT_preonic_grid',
	auto_format_pattern = '*keymap.c',
	comment_preview = {
		position = 'top',
	},
	spacing = 8,
	layout = {
		'| x x x x x x | | x x x x x x',
		'| x x x x x x | | x x x x x x',
		'| x x x x x x | | x x x x x x',
		'| x x x x x x | | x x x x x x',
		'| x x x x x x | | x x x x x x',
	},
}
