local Some = {}
local M = {}

function Some:new()
	self.__index = self
	return setmetatable({
		value = nil,
		is_nothing = true,
	}, Some)
end

function Some:map(f)
	if self.is_nothing then
		return self
	else
		self.value = f(self.value)
		return self
	end
end

function Some:or_else(v)
	if self.is_nothing then
		return v
	else
		return self.value
	end
end

-- @return Some
function M.some(v)
	local s = Some:new()
	s.value = v
	s.is_nothing = false
	return s
end

-- @return Some
function M.none() return Some:new() end

-- @return Some
function M.get_some(table, key)
	local v = table[key]
	if v == nil then
		return M.none()
	else
		return M.some(v)
	end
end

return M
