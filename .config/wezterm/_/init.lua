local _ = {
	str = require '_.str',
	list = require '_.list',
	fs = require '_.fs',
	obj = require '_.obj',
	cond = require '_.cond',

	wez = {
		_print_require_path = function()
			local str = require '_.str'
			print(str(package.path):split ';')
		end,
		---Print debug stuff
		---@param win Window
		_print_screen_info = function(win)
			-- https://github.com/wez/wezterm/pull/3006 looks like it needs building to get the actual x/y position
			local pane = win:active_pane()
			print {
				window = win:get_dimensions(),
				pane = pane:get_dimensions(),
				gui = require('wezterm').gui.screens(),
			}
		end,
	},
}

return _
