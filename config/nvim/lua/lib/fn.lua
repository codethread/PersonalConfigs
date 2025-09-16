local M = {}

---pipe data through functions
---@param ... function
---@return any
function M.pipe(...)
	local functions = { ... }
	return function(...)
		local result
		for _, func in ipairs(functions) do
			result = func(table.unpack(...))
		end
		return result
	end
end
return M
