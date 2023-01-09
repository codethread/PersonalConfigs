-- print anything and also return it
P = function(arg)
	vim.pretty_print(arg)
	return arg
end

-- print a keymap for a given lhs
-- accepts optional buffer arg to use buffer local keymaps
K = function(mode, lhs, buffer)
	local keymap
	if buffer then
		keymap = (vim.api.nvim_buf_get_keymap(buffer, mode))
	else
		keymap = (vim.api.nvim_get_keymap(mode))
	end

	for _, value in pairs(keymap) do
		if value.lhs == lhs then P(value) end
	end
end

vim.ct = {}

function vim.ct.get_visual_selection()
	-- also https://github.com/neovim/neovim/pull/13896#issuecomment-774680224

	-- Yank current visual selection into the 'v' register
	--
	-- Note that this makes no effort to preserve this register
	vim.cmd 'noau normal! "vy"'

	return vim.fn.getreg 'v'
end

function vim.ct.ft() return vim.api.nvim_exec([[echo &ft]], true) end

---@return number row, number column
function vim.ct.current_pos()
	local c = vim.api.nvim_win_get_cursor(0)
	return c[1], c[2]
end

U = require 'codethread.utils'
