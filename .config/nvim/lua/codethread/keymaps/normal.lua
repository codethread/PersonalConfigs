-- U.keymap('n', '<M-s>', '<cmd>w<cr>') -- alt or cmd on macos (terminal dependent, works with kitty)
-- expose original functionality of marks under 'M'
vim.keymap.set('n', 'M', 'm')

U.keymap('i', 'jk', '<ESC>')
U.keymap('i', 'jj', '<c-w>')
-- vim.cmd [[inoremap <C-l> <C-o>C]]
vim.cmd [[inoremap <C-d> <C-o>C]]

require('which-key').add {
	-- { 'g', group = 'stuff' },

	-- overrides marks, these get lost by most formatters
	-- making them pretty much useless
	{ 'm', group = 'harpoon' },
	-- { 'z', group = 'misc' },
}

U.keymaps({}, {

	-- don't changed jumplist with paragraph jumps :help jumplist
	-- nnoremap <silent> } :<C-u>execute "keepjumps norm! " . v:count1 . "}"<CR>
	-- nnoremap <silent> { :<C-u>execute "keepjumps norm! " . v:count1 . "{"<CR>
	-- these are for my keyboard as I have them over j & k
	{ '}', Cmd 'AerialPrev', 'next' },
	{ '{', Cmd 'AerialNex', 'prev' },

	-- NOTE: these map to plus minus on a keyboard, but might change them for my preonic
	{ '-', 'zc', 'open fold under cursor' },
	{ '=', 'zo', 'close fold under cursor' },
	{ '_', 'zC', 'close all folds under cursor' },
	{ '+', 'zO', 'open all folds under cursor' },

	--g
	{ 'ga', Cmd 'Other', 'alt file' },
	{ 'gx', function() require('codethread.gx').gx() end, 'Go to link' },
	-- ['='] -- eval stuff

	-- keep cursor centered
	{ 'n', 'nzzzv', 'Center next' },
	{ 'N', 'Nzzzv', 'Center prev' },
	{ 'J', 'mzJ`z', 'Center join' },

	--- m
	{ 'ma', function() require('harpoon.mark').add_file() end, 'harpoon.mark' },
	{ 'mf', Cmd 'Telescope harpoon marks', 'marks' },
	{ 'ml', function() require('harpoon.ui').toggle_quick_menu() end, 'harpoon.ui' },
	{ 'mk', function() require('harpoon.ui').nav_next() end, 'harpoon.ui' },
	{ 'mj', function() require('harpoon.ui').nav_prev() end, 'harpoon.ui' },
	{ 'mt', function() require('harpoon.term').gotoTerminal(1) end, 'harpoon.term' },
	{ 'm0', function() require('harpoon.ui').nav_file(0) end, 'harpoon.0' },
	{ 'm1', function() require('harpoon.ui').nav_file(1) end, 'harpoon.1' },
	{ 'm2', function() require('harpoon.ui').nav_file(2) end, 'harpoon.2' },
	{ 'm3', function() require('harpoon.ui').nav_file(3) end, 'harpoon.3' },
	{ 'm4', function() require('harpoon.ui').nav_file(4) end, 'harpoon.4' },
	{ 'm5', function() require('harpoon.ui').nav_file(5) end, 'harpoon.5' },
	{ 'm6', function() require('harpoon.ui').nav_file(6) end, 'harpoon.6' },
	{ 'm7', function() require('harpoon.ui').nav_file(7) end, 'harpoon.7' },
	{ 'm8', function() require('harpoon.ui').nav_file(8) end, 'harpoon.8' },
	{ 'm9', function() require('harpoon.ui').nav_file(9) end, 'harpoon.9' },

	-- z
	{ 'zR', function() require('ufo').openAllFolds() end, 'open all folds' },
	{ 'zM', function() require('ufo').closeAllFolds() end, 'close all folds' },
	{ 'zp', function() require('ufo').peekFoldedLinesUnderCursor() end, 'peak fold' },
	{ 'zc', function() require 'ufo' end, 'open fold under cursor' },
	{
		'zs',
		function()
			if not vim.v.count then
				vim.notify('No foldlevel given to set!', vim.log.levels.WARN)
			else
				require('codethread.fold').fold_setlevel(vim.v.count)
			end
		end,
		'set fold level',
	},

	{ 'ZQ', '<cmd>qa!<cr>', 'Quit no save' }, -- default is q!

	-- ['<C-n>'] = { Cmd 'Oil --float', 'Oil' },

	{ '<C-n>', Cmd 'Neotree reveal', 'NeoTree' },

	-- center on scroll
	{ '<C-u>', '<C-u>zz', 'Center Up' },
	{ '<C-d>', '<C-d>zz', 'Center Up' },
	{
		'<C-q>',
		function()
			for _, win in ipairs(vim.api.nvim_tabpage_list_wins(vim.api.nvim_get_current_tabpage())) do
				if vim.fn.getwinvar(win, '&syntax') == 'qf' then
					vim.cmd.cclose()
					return
				end
			end
			vim.cmd.copen()
		end,
		'Toggle quickfix',
	},
})
