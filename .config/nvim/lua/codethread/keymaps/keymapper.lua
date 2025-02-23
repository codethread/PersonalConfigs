local obj = require 'lib.obj'
local M = {}

---@class ct.KeymapCommonOpts : vim.keymap.set.Opts
---@field prefix? string
---@field mode? string|string[]

---@class ct.KeymapList
---@field [1] string # key
---@field [2] string # description
---@field [3] function | string # function or :ex command
---@field mode? string|string[]

---@class ct.KeymapDef
---@field [1] string # description
---@field [2] function | string # function or :ex command
---@field mode? string|string[]

---@class ct.KeymapTable
---@field group string?
---@field [string] ct.KeymapDef | ct.KeymapTable

---@param ... vim.keymap.set.Opts
---@return vim.keymap.set.Opts
local function defaults(...)
	return vim.tbl_extend('force', {
		noremap = true,
		nowait = true,
		silent = true,
		unique = false,
	}, ...)
end
local omitted = { 'mode' }

---Create keymaps from a table where the keys connect to make the final prefix. Groups must be annotated with `group`
---```lua
---M.create_keys('<leader>', {
---	[';'] = { 'say hi', function() vim.print 'hi' end },
---	a = {
---		group = 'group a',
---		a = { 'open buffers', 'Telescope buffers' },
---		s = { 'split', vim.fn.vsplit },
---	},
---	b = {
---		group = 'b',
---		b = {
---			group = 'nested b',
---			a = { 'open buffers', 'Telescope commands' },
---		},
---	},
---})
---```
---@param keys ct.KeymapTable
---@param prefix string
function M.tbl(prefix, keys)
	for prefix_key, group_or_def in pairs(keys) do
		-- presence of 'group' key indicates a group and not a keymap
		if group_or_def['group'] then
			M.tbl(prefix .. prefix_key, group_or_def)
		elseif prefix_key == 'group' then
			local ok, wk = pcall(require, 'which-key')
			if ok then
				wk.add { { prefix, group = group_or_def } }
			else
				vim.notify_once('which_key needed for groups', vim.log.levels.WARN)
			end
		else
			---@cast group_or_def ct.KeymapDef
			local desc, rhs = table.unpack(group_or_def)
			assert(
				desc and rhs,
				string.format("Looks like keymap wasn't given a group %s", vim.inspect(group_or_def))
			)
			local opts = defaults { desc = desc }

			vim
				.iter(group_or_def)
				:filter(
					function(option)
						return type(option) == 'string' and not vim.list_contains(omitted, option)
					end
				)
				:each(function(key, opt) opts[key] = opt end)

			Try(
				function() vim.keymap.set(group_or_def.mode or { 'n' }, prefix .. prefix_key, rhs, opts) end
			)
		end
	end
end

---Create keymaps from a list, mainly just used for nice formatting
---@param common_opts ct.KeymapCommonOpts
---@param keys ct.KeymapList[]
function M.list(common_opts, keys)
	for _, keymap in ipairs(keys) do
		local lhs, desc, rhs = table.unpack(keymap)
		lhs = (common_opts.prefix or '') .. lhs

		local opts = defaults(obj.omit(common_opts, { 'prefix', 'mode' }), { desc = desc })

		vim
			.iter(keymap)
			:filter(
				function(option) return type(option) == 'string' and not vim.list_contains(omitted, option) end
			)
			:each(function(key, opt) opts[key] = opt end)

		Try(function() vim.keymap.set(keymap.mode or common_opts.mode or { 'n' }, lhs, rhs, opts) end)
	end
end

---@param keys ct.KeymapList[]
function M.localleader(keys)
	local buf = vim.api.nvim_get_current_buf()
	M.list({ buffer = buf, prefix = '<localleader>' }, keys)
end

---Create local bindings for a filetype
---@param filetype string,
---@param keys ct.KeymapList[]
function M.localleader_ft(filetype, keys)
	vim.api.nvim_create_autocmd('FileType', {
		pattern = filetype,
		callback = function() M.localleader(keys) end,
	})
end

---Create local bindings for a file by name
---@param filename string,
---@param keys ct.KeymapList[]
function M.localleader_pat(filename, keys)
	vim.api.nvim_create_autocmd('BufEnter', {
		pattern = filename,
		callback = function() M.localleader(keys) end,
	})
end

return M
