---JavaScript - I love it
---@class Str
---@field private value string
local Str = {}

---Create String instance
---@package
---@param value string
---@return Str
function Str.new(value)
	if type(value) ~= "string" then
		error("value must be string,")
	end
	---@type Str
	local newStr = { value = value }
	local obj = setmetatable(newStr, { __index = Str })
	return obj
end

---Does Str contain subtring
---@param substring string
---@return boolean
function Str:contains(substring)
	return self.value:find(substring, 1, true) ~= nil
end

---@param start string
---@return boolean
function Str:starts_with(start)
	return self.value:sub(1, #start) == start
end

---@param ending string
---@return boolean
function Str:ends_with(ending)
	return ending == "" or self.value:sub(-#ending) == ending
end

---Replace substring `old` with `new`
---@param old string
---@param new string
---@return Str
function Str:replace(old, new)
	local s = self.value
	local search_start_idx = 1

	while true do
		local start_idx, end_idx = s:find(old, search_start_idx, true)
		if not start_idx then
			break
		end

		local postfix = s:sub(end_idx + 1)
		s = s:sub(1, (start_idx - 1)) .. new .. postfix

		search_start_idx = -1 * postfix:len()
	end

	return Str.new(s)
end

---@return integer
function Str:length()
	return #self.value
end

---Method to mimic JavaScript's String.prototype.charAt
---@param index number: The index of the character to retrieve
---@return string The character at the specified index
function Str:char_at(index)
	return self.value:sub(index + 1, index + 1) ---Lua uses 1-based indexing
end

---Method to mimic JavaScript's String.prototype.indexOf
---@param search string The substring to search for
---@param fromIndex number|nil The index to start searching from
---@return number The index of the first occurrence of the substring or -1 if not found
function Str:index_of(search, fromIndex)
	local start = (fromIndex or 0) + 1 ---Lua's string functions use 1-based index
	local pos = self.value:find(search, start, true)
	return pos and (pos - 1) or -1 ---Return 0-based index or -1 if not found
end

---Method to mimic JavaScript's String.prototype.slice
---@param start number The starting index
---@param stop number|nil The ending index (exclusive)
---@return Str A new String object containing the sliced part
function Str:slice(start, stop)
	return Str.new(self.value:sub(start + 1, stop and stop + 1))
end

---Method to mimic JavaScript's String.prototype.toUpperCase
---@return Str A new String object with all characters converted to uppercase
function Str:to_upper_case()
	return Str.new(self.value:upper())
end

---Method to mimic JavaScript's String.prototype.toLowerCase
---@return Str A new String object with all characters converted to lowercase
function Str:to_lower_case()
	return Str.new(self.value:lower())
end

---Method to mimic JavaScript's String.prototype.trim
---@return Str A new String object with whitespace removed from both ends
function Str:trim()
	return Str.new(self.value:gsub("^%s*(.-)%s*$", "%1"))
end

function Str:to_string()
	return self.value
end

return setmetatable({}, {
	__call = function(_, tbl)
		return Str.new(tbl)
	end,
	__index = Str,
})
