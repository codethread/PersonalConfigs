---@diagnostic disable: missing-fields
local wezterm = require 'wezterm' --[[@as Wezterm]]

local M = {}

---@param config Config
function M.apply_to_config(config)
	-- For example, changing the color scheme:
	config.color_scheme = 'rose-pine-moon'

	config.window_decorations = 'RESIZE'
	-- config.enable_tab_bar = false
	-- tab bar
	-- config.underline_thickness = 2
	config.tab_bar_at_bottom = true
	-- config.use_fancy_tab_bar = false
	-- config.tab_and_split_indices_are_zero_based = true
	config.line_height = 1.5
	config.font_size = 13.0
	config.underline_position = -4
	config.font = wezterm.font_with_fallback {
		{
			family = 'FiraCode Nerd Font',
			weight = 'Medium',
			harfbuzz_features = {
				'salt=2',
				'cv01=1',
				'cv02=1',
				'cv06=1',
				'cv14=1',
				'+zero',
				'+onum',
				'+ss04',
				'cv18=1',
				'cv30=1',
				'+ss09',
				'+ss07',
			},
		},
		{
			family = 'Symbols Nerd Font Mono',
		},
	}

	local theme = wezterm.plugin.require('https://github.com/neapsix/wezterm').moon
	config.window_frame = theme.window_frame()
	config.hide_tab_bar_if_only_one_tab = true
end

return M
