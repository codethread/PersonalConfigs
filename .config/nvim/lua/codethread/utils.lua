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

---call pcall on multiple modules
---good to use @module on the return for types
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

local init_buf = vim.api.nvim_create_buf(false, true)
vim.api.nvim_buf_set_name(init_buf, 'init')
vim.api.nvim_create_user_command('OpenInitBuffer', 'buffer init', {})

local failed_modules = require('plenary.collections.py_list').new {}

---
---Loads the given module, using pcall, returns any value returned by the given module(`true` when `nil`) plus ok boolean
---
---The name is kept the same to kep sumneko_lua happy and give module types
---
---[View documents](http://www.lua.org/manual/5.1/manual.html#pdf-require)
---
---@param modname string
---@return unknown, boolean
function M.require(modname)
	local ok, err = pcall(require, modname)
	if not ok then failed_modules:push(err) end
	local msg = ('info: ' .. (ok and 'loaded' or 'failed to load') .. ' ' .. modname)
	vim.api.nvim_buf_set_lines(init_buf, -1, -1, false, { msg })
	return err, ok
end

function M.get_failed_modules()
	if #failed_modules ~= 0 then print(#failed_modules, 'module(s) failed to load') end
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

function M.telescope_hook(extension)
	vim.api.nvim_create_autocmd('User', {
		desc = 'setup telescope plugin for ' .. extension,
		group = vim.api.nvim_create_augroup('MyTelescope', {}),
		pattern = { 'TelescopeLoaded' },
		callback = function() require('telescope').load_extension(extension) end,
	})
end

function M.log(...) print('info: ', ...) end

function M.cmd(command) return '<Cmd>' .. command .. '<CR>' end

return M
