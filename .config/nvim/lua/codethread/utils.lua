-- Keybindings / mappings
local function map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then options = vim.tbl_extend('force', options, opts) end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local function nmap(lhs, rhs, opts) map('n', lhs, rhs, opts) end

local function vmap(lhs, rhs, opts) map('v', lhs, rhs, opts) end

local function command(desc, lhs, rhs, arg_count)
	vim.api.nvim_buf_create_user_command(arg_count or 0, lhs, rhs, { desc = desc })
end

return {
	map = map,
	nmap = nmap,
	vmap = vmap,
	command = command,
}
