local require = require('codethread.utils').require
local aerial, ok = require 'aerial'
local telescope, ok_t = require 'codethread.telescope'
if not ok then return end

aerial.setup {
	layout = {
		min_width = 15,
		default_direction = 'prefer_left',
	},
	-- optionally use on_attach to set keymaps when aerial has attached to a buffer
	on_attach = function(bufnr)
		-- Jump forwards/backwards with '{' and '}'
		vim.keymap.set('n', '{', '<cmd>AerialPrev<CR>', { buffer = bufnr })
		vim.keymap.set('n', '}', '<cmd>AerialNext<CR>', { buffer = bufnr })
	end,
}

if ok_t then telescope.load_extension 'aerial' end
