U = require 'config.utils'

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
		if value.lhs == lhs then vim.print(value) end
	end
end

function Cmd(command) return '<Cmd>' .. command .. '<CR>' end

function Lua(command) return '<Cmd>lua ' .. command .. '<CR>' end

function Term() end
