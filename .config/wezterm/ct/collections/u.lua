local u = {}

---@generic A
---@generic B
---@param tbl A[]
---@param fn fun(val: A, i: integer, tbl: A[]): B
---@return B[]
function u.map(tbl, fn)
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
function u.filter(tbl, fn) return error 'no impl' end

---@generic A
---@param tbl A[]
---@param fn fun(val: A, i: integer, tbl: A[]): boolean
---@return A|nil
function u.find(tbl, fn)
	for i = 1, #tbl do
		if fn(tbl[i], i, tbl) then return tbl[i] end
	end
	return nil
end

return u
