local require = require('codethread.utils').require
local qmk, ok = require 'qmk'
if not ok then return end

local group = vim.api.nvim_create_augroup('MyQMK', {})

vim.api.nvim_create_autocmd('BufEnter', {
	desc = 'Setup preonic',
	group = group,
	pattern = '*keymap.c',
	callback = function()
		---@type qmk.UserConfig
		local config = {
			name = 'LAYOUT_preonic_grid',
			comment_preview = {
				keymap_overrides = {
					_______ = '␣',
				},
			},
			layout = {
				'_ x x x x x x _ x x x x x x',
				'_ x x x x x x _ x x x x x x',
				'_ x x x x x x _ x x x x x x',
				'_ x x x x x x _ x x x x x x',
				'_ x x x x x x _ x x x x x x',
			},
		}
		qmk.setup(config)
	end,
})

vim.api.nvim_create_autocmd('BufEnter', {
	desc = 'Setup alternate keymap',
	group = group,
	pattern = '*foo.keymap',
	callback = function()
		qmk.setup {
			name = 'LAYOUT_preonic_grid',
			variant = 'zmk',
			layout = {
				'x x x x x x x _ _ _ _ _ _ _ _ _ _ x x x x x x x',
				'x x x x x x x _ _ _ _ _ _ _ _ _ _ x x x x x x x',
				'x x x x x x x _ x x x _ _ x x x _ x x x x x x x',
				'x x x x x x _ x x x x _ _ x x x x _ x x x x x x',
				'x x x x x _ _ _ x x x _ _ x x x _ _ _ x x x x x',
			},
		}
	end,
})
