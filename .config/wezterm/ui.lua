---@diagnostic disable: missing-fields
local wezterm = require 'wezterm' --[[@as Wezterm]]
local theme = require 'theme'

local M = {}

-- It prefers the title that was set via `tab:set_title()` but falls back
-- to the title of the active pane in that tab.
--
-- https://wezfurlong.org/wezterm/config/lua/window-events/format-tab-title.html
local function format_tabs()
	local function tab_title(tab_info)
		local title = tab_info.tab_title
		if title and #title > 0 then return title end
		return tab_info.active_pane.title
	end

	wezterm.on('format-tab-title', function(tab, tabs, panes, config, hover, max_width)
		local title = tab_title(tab)
		if tab.is_active then
			return {
				-- { Background = { Color = 'blue' } },
				{ Text = ' ' .. title .. ' ' },
			}
		end
		return ' ' .. title .. ' '
	end)
end

---@param config Config
function M.apply_to_config(config)
	config.window_decorations = 'RESIZE'
	-- config.tab_and_split_indices_are_zero_based = true
	config.line_height = 1.4
	config.window_padding = {
		left = 2,
		right = 2,
		top = 2,
		bottom = 0,
	}
	config.underline_position = -4
	-- config.underline_thickness = 2

	config.font_size = 13.0
	config.font = wezterm.font_with_fallback {
		{
			-- family = 'Liga Hack',
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

	config.tab_bar_at_bottom = true
	config.use_fancy_tab_bar = false
	config.tab_max_width = 100 -- non-fancy
	config.color_scheme = 'rose-pine-moon'
	config.colors = theme.colors()
	config.window_frame = theme.window_frame()

	format_tabs()
end

return M
