local list = {}

---@generic A
---@param ... A[] # list like tables to concat
---@return A[]
function list.concat(...)
	require 'table.new'
	local size = 0
	local tbls = {}
	for _, tbl in ipairs { ... } do
		size = size + #tbl
		table.insert(tbls, tbl)
	end
	local result = table.new(size, 0)
	for _, tbl in ipairs(tbls) do
		for _, item in ipairs(tbl) do
			table.insert(result, item)
		end
	end
	return result
end

return list
