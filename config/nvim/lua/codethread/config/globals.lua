U = require 'codethread.config.utils'

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

---Print a value and return it (tap)
---@generic A
---@param ... A
---@return A
P = function(...)
	vim.print(...)
	return ...
end

---inline an ex command, handy for keymaps
---@param command string
---@return string
function Cmd(command) return '<Cmd>' .. command .. '<CR>' end

---inline some lua code, handle for keymaps
---@param command string
---@return string
function Lua(command) return '<Cmd>lua ' .. command .. '<CR>' end

Keys = require 'codethread.keymaps.keymapper'

---@param fn function
local function try_fn(fn)
	local ok, err = pcall(fn)
	if not ok then vim.notify(err, 'error') end
end

---`pcall` the function and log an error if fails, mainly intended for things
---like ftplugin/* which will fail silently
---@param fns function | function[] # closure to run
function Try(fns)
	if type(fns) == 'table' then
		for _, fn in ipairs(fns) do
			try_fn(fn)
		end
	else
		try_fn(fns)
	end
end

function Term() end
