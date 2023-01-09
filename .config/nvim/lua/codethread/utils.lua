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

---call pcall on multiple modules, returning
---@param lib_tbl string[]
---@return string[]
---@return any ...
function M.requires(lib_tbl)
	local errs = {}
	local modules = {}

	for _, lib in pairs(lib_tbl) do
		local m_status_ok, m = pcall(require, lib)
		if not m_status_ok then
			table.insert(errs, m)
		else
			table.insert(modules, m)
		end
	end

	if not #errs == 0 then return errs end

	local c = #modules
	if c == 1 then
		return nil, modules[1]
	elseif c == 2 then
		return nil, modules[1], modules[2]
	elseif c == 3 then
		return nil, modules[1], modules[2], modules[3]
	elseif c == 4 then
		return nil, modules[1], modules[2], modules[3], modules[4]
	elseif c == 5 then
		return nil, modules[1], modules[2], modules[3], modules[4], modules[5]
	elseif c == 6 then
		return nil, modules[1], modules[2], modules[3], modules[4], modules[5], modules[6]
	end
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
