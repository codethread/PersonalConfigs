return {
	-- don't changed jumplist with paragraph jumps :help jumplist
	-- nnoremap <silent> } :<C-u>execute "keepjumps norm! " . v:count1 . "}"<CR>
	-- nnoremap <silent> { :<C-u>execute "keepjumps norm! " . v:count1 . "{"<CR>
	-- these are for my keyboard as I have them over j & k
	['{'] = { Cmd 'AerialPrev', 'next' },
	['}'] = { Cmd 'AerialNex', 'prev' },

	['-'] = { 'zc', 'open fold under cursor' },
	['='] = { 'zo', 'close fold under cursor' },
	['_'] = { 'zC', 'close all folds under cursor' },
	['+'] = { 'zO', 'open all folds under cursor' },

	-- keep cursor centered
	n = { 'nzzzv', 'Center next' },
	N = { 'Nzzzv', 'Center prev' },
	J = { 'mzJ`z', 'Center join' },

	M = 'm', -- expose original functionality of marks under 'M'
	m = {
		-- overrides marks, these get lost by most formatters
		-- making them pretty much useless
		name = 'harpoon',
		a = { function() require('harpoon.mark').add_file() end, 'harpoon.mark' },
		f = { Cmd 'Telescope harpoon marks', 'marks' },
		l = { function() require('harpoon.ui').toggle_quick_menu() end, 'harpoon.ui' },
		k = { function() require('harpoon.ui').nav_next() end, 'harpoon.ui' },
		j = { function() require('harpoon.ui').nav_prev() end, 'harpoon.ui' },
		t = { function() require('harpoon.term').gotoTerminal(1) end, 'harpoon.term' },
		['0'] = { function() require('harpoon.ui').nav_file(0) end, 'harpoon.0' },
		['1'] = { function() require('harpoon.ui').nav_file(1) end, 'harpoon.1' },
		['2'] = { function() require('harpoon.ui').nav_file(2) end, 'harpoon.2' },
		['3'] = { function() require('harpoon.ui').nav_file(3) end, 'harpoon.3' },
		['4'] = { function() require('harpoon.ui').nav_file(4) end, 'harpoon.4' },
		['5'] = { function() require('harpoon.ui').nav_file(5) end, 'harpoon.5' },
		['6'] = { function() require('harpoon.ui').nav_file(6) end, 'harpoon.6' },
		['7'] = { function() require('harpoon.ui').nav_file(7) end, 'harpoon.7' },
		['8'] = { function() require('harpoon.ui').nav_file(8) end, 'harpoon.8' },
		['9'] = { function() require('harpoon.ui').nav_file(9) end, 'harpoon.9' },
	},

	z = {
		name = 'misc',
		-- R = { function() require('ufo').openAllFolds() end, 'open all folds' },
		-- M = { function() require('ufo').closeAllFolds() end, 'close all folds' },
		-- r = { function() require('ufo').openFoldsExceptKinds() end, 'descrease fold' },
		-- m = { function() require('ufo').closeFoldsWith() end, 'increase fold' },
	},

	['<C-n>'] = { Cmd 'Oil --float', 'Oil' },

	-- center on scroll
	['<C-u>'] = { '<C-u>zz', 'Center Up' },
	['<C-d>'] = { '<C-d>zz', 'Center Up' },
}
