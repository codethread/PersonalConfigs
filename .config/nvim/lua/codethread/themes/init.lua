local events = require 'codethread.events'
-- local nord = require 'codethread.themes.nord'
local tokyo = require 'codethread.themes.tokyonight'
require('codethread.themes.setup').setup(tokyo)

return {
	lualine = tokyo.statusline,
	colors = tokyo.colors,
	---@param cb fun(mode: 'light' | 'dark', colors: ColorScheme): nil
	on_change = function(cb)
		-- cb('dark', tokyo.colors())
		events.on('ColorScheme', function(mode, colors) cb(mode, colors) end)
	end,
}
