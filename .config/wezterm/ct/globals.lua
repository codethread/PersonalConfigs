local wezterm = require("wezterm") --[[@as Wezterm]]

local M = {}

function M.setup()
	---Not present in wezterm, but if using things from luarocks, they may expect this
	debug = require("debug")

	---Run a list of functions and wrap them all in pcalls
	---@param fns table
	Try = function(fns)
		for _, fn in ipairs(fns) do
			local success, okOrErr = pcall(fn)
			if not success then
				wezterm.log_error(okOrErr)
			end
		end
	end
end

return M
