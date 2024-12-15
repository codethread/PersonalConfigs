local list = {}

---@generic A
---@generic B
---@param tbl A[]
---@param fn fun(val: A, i: integer, tbl: A[]): B
---@return B[]
function list.map(tbl, fn)
	local result = {}
	for i = 1, #tbl do
		table.insert(result, i, fn(tbl[i], i, tbl))
	end
	return result
end

---@generic A
---@param tbl A[]
---@param fn fun(val: A, i: integer, tbl: A[]): boolean
---@return A[]
function list.filter(tbl, fn)
	local result = {}
	for i = 1, #tbl do
		if fn(tbl[i], i, tbl) then table.insert(result, tbl[i]) end
	end
	return result
end

---@generic A
---@param tbl A[]
---@param fn fun(val: A, i: integer, tbl: A[]): boolean
---@return A|nil
function list.find(tbl, fn)
	for i = 1, #tbl do
		if fn(tbl[i], i, tbl) then return tbl[i] end
	end
	return nil
end

---Collect an iterator `fn` into a list
---@generic A
---@param fn fun(): A, ...any
---@return A[]
function list.from_iter(fn)
	local values = {}
	-- this may not be the best
	for value in fn() do
		table.insert(values, value)
	end
	return values
end

function list.flatten(tbl)
	local flat = {}
	for _, nested in ipairs(tbl) do
		for _, item in ipairs(nested) do
			table.insert(flat, item)
		end
	end
	return flat
end

function list.index_of(tbl, item)
	for i = 1, #tbl do
		if tbl[i] == item then return i end
	end
	return nil
end

function list.uniq(tbl)
	return list.filter(tbl, function(item, i) return list.index_of(tbl, item) == i end)
end

return list
