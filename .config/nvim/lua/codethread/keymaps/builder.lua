local M = {}

---@class ct.KeymapDef
---@field [1] string # description
---@field [2] function | string # function or :ex command
---@field mode? string|string[]

---@class ct.KeymapGroup
---@field group string?
---@field [string] ct.KeymapDef | ct.KeymapGroup

---Create keymaps from a table where the keys connect to make the finale prefix. Groups must be annotated with `group`
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
---@param keys ct.KeymapGroup
---@param prefix string
---@param _depth? number
function M.create_keys(prefix, keys, _depth)
	if _depth > 100 then error 'iteration too high, probably caused an infinite loop' end

	for prefix_key, group_or_def in pairs(keys) do
		-- presence of 'group' key indicates a group and not a keymap
		-- TODO: how to validate abscence
		if group_or_def['group'] then
			M.create_keys(prefix .. prefix_key, group_or_def, _depth + 1)
		elseif prefix_key == 'group' then
			local ok, wk = pcall(require, 'which-key')
			if ok then
				wk.add { { prefix, group = group_or_def } }
			else
				vim.notify_once('which_key needed for groups', vim.log.levels.WARN)
			end
		else
			---@cast group_or_def ct.KeymapDef
			assert(
				group_or_def[1],
				string.format("Looks like keymap wasn't given a group %s", group_or_def)
			)

			---@type vim.keymap.set.Opts
			local defaults = { silent = true, unique = false, desc = group_or_def[1] }
			-- local defaults = { silent = true, unique = true }
			for key, opt in pairs(group_or_def) do
				if type(key) ~= 'number' then defaults[key] = opt end
			end

			Try(
				function()
					vim.keymap.set(
						group_or_def.mode or { 'n' },
						prefix .. prefix_key,
						group_or_def[2],
						defaults
					)
				end
			)
		end
	end
end

return M
