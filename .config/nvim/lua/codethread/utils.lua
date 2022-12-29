local M = {}

-- Keybindings / mappings
function M.map(mode, lhs, rhs, opts)
	local options = { noremap = true }
	if opts then options = vim.tbl_extend('force', options, opts) end
	vim.keymap.set(mode, lhs, rhs, options)
end

function M.nmap(lhs, rhs, opts) M.map('n', lhs, rhs, opts) end
function M.imap(lhs, rhs, opts) M.map('i', lhs, rhs, opts) end
function M.vmap(lhs, rhs, opts) M.map('v', lhs, rhs, opts) end

function M.command(desc, lhs, rhs, arg_count)
	vim.api.nvim_buf_create_user_command(arg_count or 0, lhs, rhs, { desc = desc })
end

function M.hl(color_table, ns)
	for group, opts in pairs(color_table) do
		vim.api.nvim_set_hl(ns or 0, group, opts)
	end
end

function M.safe_load(lib, fn)
	local status_ok, loaded_lib = pcall(require, lib)
	if not status_ok then
		vim.notify('could not load ' .. lib)
		return
	end
	return fn(loaded_lib)
end

function M.autocmd(events, opts)
	vim.api.nvim_clear_autocmds { group = opts.group, buffer = opts.buffer }
	vim.api.nvim_create_autocmd(events, {
		group = opts.group,
		buffer = opts.buffer,
		callback = opts.fn,
	})
end

function M.file_exits(path)
	local p = vim.fs.normalize(path)
	return vim.fn.isdirectory(p) == 1
end

return M
