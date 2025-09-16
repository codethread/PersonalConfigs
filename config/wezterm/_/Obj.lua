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
---@param keys string[]
---@return table
function Obj.pick(tbl, keys)
	local picked = {}
	for key, value in pairs(tbl) do
		for _, picked_key in ipairs(keys) do
			if key == picked_key then
				picked[key] = value
				break
			end
		end
	end
	return picked
end

return Obj
