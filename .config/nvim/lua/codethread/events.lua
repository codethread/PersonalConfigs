local List = require 'plenary.collections.py_list'
local M = {}

local global_handlers = {}

M.on = function(event, handler)
	---@type List
	local events = global_handlers[event] or List:new()
	events:push(handler)
end

M.emit = function(event, ...)
	local events = global_handlers[event] or List:new()
	for _, handler in ipairs(events) do
		handler(...)
	end
end

return M
