---@class Array
---@field private value table
local Arr = {}

---@package
---@param list table
---@return Array
function Arr.new(list)
	if type(list) ~= "table" then
		error("Array constructor must be called with table argument")
	end

	local len = #list
	local obj = setmetatable(list, Arr)
	obj._len = len
	return obj
end

---Is the current list is an instance of an `Array`
---@param list Array
---@return boolean
function Arr.is_array(list)
	local meta = getmetatable(list) or {}
	return meta == Arr
end

---Adds one or more elements to the end of the array. Returning new length of array
---@param ... unknown
---@return integer
function Arr:push(...)
	for _, value in ipairs({ ... }) do
		self[#self + 1] = value
	end
	return #self
end

---Removes and returns the last element.
---Mutates array
---@return any
function Arr:pop()
	return table.remove(self)
end

---Removes and returns the first element.
---@return any
function Arr:shift()
	return table.remove(self, 1)
end

---Adds one element to the beginning of the array.
---comment
---@param value any
---@return nil
function Arr:unshift(value)
	table.insert(self, 1, value)
end

---Concatenates another array to this one.
---@param other Array
---@return Array
function Arr:concat(other)
	local result = Arr.new(self.value)
	result:push(table.unpack(other.value))
	return result
end

---Creates a new array with the results of calling a provided function on every
---element in the calling array.
---@param fn fun(any, integer, Array): any
---@return Array
function Arr:map(fn)
	local result = Arr.new({})
	for i = 1, #self do
		result:push(fn(self[i], i, self))
	end
	return result
end

---Creates a list table by applying a mapping function `fn` to the Array (Similar to Array.map but does not return a new Array instance)
---@generic T
---@param fn fun(any, integer, Array): T
---@return T[]
function Arr:map_list(fn)
	local result = {}
	for i = 1, #self do
		result[i] = fn(self[i], i, self)
	end
	return result
end

---Creates a new array with all elements that pass the test implemented by the
---provided function.
---@param fn fun(any, integer, Array): boolean
---@return Array
function Arr:filter(fn)
	local result = Arr.new({})
	for i = 1, #self do
		if fn(self[i], i, self) then
			result:push(self[i])
		end
	end
	return result
end

---Executes a provided function once for each array element.
---@param fn fun(any, integer, Array): nil
function Arr:forEach(fn)
	for i = 1, #self do
		fn(self[i], i, self)
	end
end

---Joins all elements of the array into a string, separated by the specified separator
---@param separator string
---@return string
function Arr:join(separator)
	separator = separator
	return table.concat(self, separator)
end

---Array Applies a function against an accumulator and each element in the array (from left to right) to reduce it to a single value.
---@generic T
---@param fn fun(T, any, integer, Array): T
---@param initial T
---@return T
function Arr:reduce(fn, initial)
	local accumulator = initial
	for i = 1, #self do
		accumulator = fn(accumulator, self[i], i, self)
	end
	return accumulator
end

function Arr:find(predicate)
	for i = 1, #self do
		if predicate(self[i], i, self) then
			return self[i]
		end
	end
	return nil
end

return setmetatable({}, {
	__call = function(_, tbl)
		return Arr.new(tbl)
	end,
	__index = Arr,
})
