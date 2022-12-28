local status_ok, toggleterm = pcall(require, 'toggleterm')
if not status_ok then return end

local theme = require('codethread.themes.nord').colors()

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
			guibg = theme.dark,
		},
		NormalFloat = {
			link = 'NormalDark',
		},
		FloatBorder = {
			guifg = theme.dark,
			guibg = theme.dark,
		},
	},
	float_opts = {
		border = 'single',
		winblend = 3,
	},
}

function _G.set_terminal_keymaps()
	local opts = { noremap = true }
	vim.api.nvim_buf_set_keymap(0, 't', '<esc>', [[<C-\><C-n>]], opts)
	vim.api.nvim_buf_set_keymap(0, 't', 'jk', [[<C-\><C-n>]], opts)
	vim.api.nvim_buf_set_keymap(0, 't', '<C-h>', [[<C-\><C-n><C-W>h]], opts)
	vim.api.nvim_buf_set_keymap(0, 't', '<C-j>', [[<C-\><C-n><C-W>j]], opts)
	vim.api.nvim_buf_set_keymap(0, 't', '<C-k>', [[<C-\><C-n><C-W>k]], opts)
	vim.api.nvim_buf_set_keymap(0, 't', '<C-l>', [[<C-\><C-n><C-W>l]], opts)
end

-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd 'autocmd! TermOpen term://* lua set_terminal_keymaps()'

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
