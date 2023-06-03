local require = require('codethread.utils').require
local toggleterm, ok = require 'toggleterm'
if not ok then return end

require('codethread.themes').on_change(function(_, colors)
	toggleterm.setup {
		-- size can be a number or function which is passed the current terminal
		size = function(term)
			if term.direction == 'horizontal' then
				return 15
			elseif term.direction == 'vertical' then
				return vim.o.columns * 0.4
			end
		end,
		open_mapping = [[<C-\>]],
		hide_numbers = true, -- hide the number column in toggleterm buffers
		start_in_insert = true,
		insert_mappings = true, -- whether or not the open mapping applies in insert mode
		terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
		persist_size = true,
		direction = 'horizontal',
		close_on_exit = false, -- close the terminal window when the process exits
		highlights = {
			Normal = {
				guibg = colors.dark,
			},
			NormalFloat = {
				link = 'NormalDark',
			},
			FloatBorder = {
				guifg = colors.dark,
				guibg = colors.dark,
			},
		},
		float_opts = {
			border = 'single',
			winblend = 3,
		},
	}
end)

local Term = require('toggleterm.terminal').Terminal

local node = Term:new { cmd = 'node', hidden = true, close_on_exit = true }
local format_dotfiles = Term:new {
	cmd = "stylua --glob '**/*.lua' -- .config/nvim",
	dir = os.getenv 'DOTFILES',
	close_on_exit = true,
	direction = 'horizontal',
}

function _NODE_TOGGLE() node:toggle() end

function _FORMAT_DOTFILES() format_dotfiles:toggle() end
