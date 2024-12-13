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
