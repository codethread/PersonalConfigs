local require = require('codethread.utils').require
local qmk, ok = require 'qmk'
if not ok then return end

qmk.setup {
	name = 'LAYOUT_preonic_grid',
	layout = {
		'_ x x x x x x _ x x x x x x',
		'_ x x x x x x _ x x x x x x',
		'_ x x x x x x _ x x x x x x',
		'_ x x x x x x _ x x x x x x',
		'_ x x x x x x _ x x x x x x',
	},
}
