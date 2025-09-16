---@class Obj
local Obj = {}

---Get keys of object
---@param tbl any
---@return string[]
function Obj.keys(tbl)
	local keys = {}
	for key in pairs(tbl) do
		table.insert(keys, key)
	end
	return keys
end

---Get values of object
---@param tbl any
---@return any
function Obj.values(tbl)
	local keys = {}
	for _, value in pairs(tbl) do
		table.insert(keys, value)
	end
	return keys
end

---Pick a list of keys from tbl, returning a new table
---@param tbl table
---@param keys string[] list of keys to pick from table (order controls the shape of the new table)
---@param renames? table<string, string> an optional record of key names to remap to a new label
---@return table
function Obj.pick(tbl, keys, renames)
	renames = renames or {}
	local picked = {}
	for key, value in pairs(tbl) do
		for _, picked_key in ipairs(keys) do
			if key == picked_key then
				local name = renames[key] or key
				picked[name] = value
				break
			end
		end
	end
	return picked
end

---@class obj.omit.opts
---@field indicate_hidden? boolean Replace omitted keys with value `[key] = '<OMITTED>'`, intended for logging

---Remove `keys` from `tbl` assuming tbl to be a dict
---@param tbl table
---@param keys string[]
---@param opts? obj.omit.opts
function Obj.omit(tbl, keys, opts)
	opts = opts or {}

	if getmetatable(tbl) then
		-- NOTE: can extend this if needed, but by default `pairs` won't get metadata, and if omitting keys, this could lead to odd behaviour
		error "can't omit keys from a class, if just logging, use `obj.to_inspectable"
	end

	local out = {}
	for key, value in pairs(tbl) do
		if not vim.list_contains(keys, key) then
			out[key] = value
		elseif opts.indicate_hidden then
			out[key] = '<OMITTED>'
		end
	end
	return out
end

local primitives = {
	'nil',
	'number',
	'string',
	'boolean',
	'function',
	'thread',
	'userdata',
}

---@class obj.inspect.opts
---@field omit? string[] Keys to omit from the inspectable object
---@field indicate_hidden? boolean Replace omitted keys with value `[key] = '<OMITTED>'`, intended for logging

---Gets an object for vim.inspect but removes meta data and `keys`
---@param tbl table
---@param opts? obj.inspect.opts
function Obj.to_inspectable(tbl, opts)
	opts = opts or {}
	opts.omit = opts.omit or {}

	local out = {}

	if getmetatable(tbl) then out.__meta = '<HAS META>' end

	for key, value in pairs(tbl) do
		local should_omit = vim.list_contains(opts.omit, key)

		if should_omit then
			if opts.indicate_hidden then out[key] = '<OMITTED>' end
			goto continue
		end

		local is_printable = vim.list_contains(primitives, type(value)) or vim.islist(value)

		if is_printable then
			out[key] = value
		else
			out[key] = Obj.to_inspectable(value, opts)
		end

		::continue::
	end

	return out
end

return Obj
