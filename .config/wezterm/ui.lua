---@diagnostic disable: missing-fields
local wezterm = require 'wezterm' --[[@as Wezterm]]
local theme = require 'theme'

local M = {}

wezterm.on('update-right-status', function(window, _)
	local name = window:mux_window():get_workspace()
	local text = wezterm.nerdfonts.md_folder_marker .. ' ' .. name .. '  '

	window:set_left_status(wezterm.format {
		{
			Foreground = {
				Color = window:leader_is_active() and theme.palette.iris or theme.palette.base,
			},
		},
		{
			Background = { Color = theme.palette.base },
		},
		{ Text = '' },

		'ResetAttributes',
		{ Attribute = { Intensity = 'Bold' } },
		{
			Foreground = {
				Color = window:leader_is_active() and theme.palette.base or theme.palette.iris,
			},
		},
		{
			Background = {
				Color = window:leader_is_active() and theme.palette.iris or theme.palette.base,
			},
		},
		{ Text = text },
		'ResetAttributes',

		{
			Foreground = {
				Color = window:leader_is_active() and theme.palette.iris or theme.palette.base,
			},
		},
		{
			Background = { Color = theme.palette.base },
		},
		{ Text = window:leader_is_active() and '' or '' },
	})
end)

-- It prefers the title that was set via `tab:set_title()` but falls back
-- to the title of the active pane in that tab.
--
-- https://wezfurlong.org/wezterm/config/lua/window-events/format-tab-title.html
local function format_tabs()
	---@param tab_info TabInformation
	local function tab_title(tab_info)
		local title = tab_info.tab_title
		local z = tab_info.active_pane.is_zoomed and '+ ' or ''
		if title and #title > 0 then return title .. z end
		return tab_info.active_pane.title .. z
	end

	wezterm.on('format-tab-title', function(tab, tabs, panes, config, hover, max_width)
		local title = tab_title(tab)
		if tab.is_active then
			return wezterm.format {
				-- { Background = { Color = 'blue' } },
				-- { Text = ' ' .. title .. ' ' },
				{ Attribute = { Intensity = 'Bold' } },
				{ Text = title },
			}
		end
		return wezterm.format {
			{ Text = title },
		}
	end)
end

---@param config Config
--- https://wezfurlong.org/wezterm/config/appearance.html#native-fancy-tab-bar-appearance
local function format_fancy(config)
	-- config.show_close_tab_button_in_tabs = false
	config.tab_bar_at_bottom = false
	config.use_fancy_tab_bar = true
	config.window_frame = {
		font = config.font,
		font_size = config.font_size,
		active_titlebar_bg = theme.colors.background,
		inactive_titlebar_bg = theme.colors.background,
	}
end

---@param config Config
local function format_non_fancy(config)
	config.tab_bar_at_bottom = false
	config.tab_max_width = 100 -- non-fancy
	config.use_fancy_tab_bar = false
end

local SIZE = {
	LARGE = 'large',
	CRISP = 'CRISP',
}

wezterm.on('window-config-reloaded', function(window)
	local gui = wezterm.gui
	if gui then
		local dpi = gui.screens().main.effective_dpi > 100 and SIZE.CRISP or SIZE.LARGE

		local overrides = window:get_config_overrides() or {}
		if dpi == SIZE.CRISP then
			overrides.harfbuzz_features = {
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
			}
			overrides.underline_position = -4
		else
			overrides.underline_position = nil
			overrides.harfbuzz_features = {
				'calt=1',
				'clig=1',
				'liga=1',

				-- '+zero',
				-- '+onum',

				-- 'salt=2', -- doing weird things
				-- 'cv01=1',
				-- 'cv02=1',
				-- 'cv06=1',
				-- 'cv14=1',
				-- '+ss04',
				-- 'cv18=1',
				-- 'cv30=1',
				-- '+ss09',
				-- '+ss07',
			}
		end
		-- NOTE: probably just want to hash function changes
		local overrides_hash = wezterm.json_encode { [dpi] = overrides }
		if wezterm.GLOBAL.overrides_hash == overrides_hash then return end
		print('font changes', overrides_hash)
		wezterm.GLOBAL.overrides_hash = overrides_hash
		window:set_config_overrides(overrides)
	end
end)

---@param config Config
function M.apply_to_config(config)
	config.window_decorations = 'RESIZE'
	-- config.tab_and_split_indices_are_zero_based = true
	config.line_height = 1.4
	config.window_padding = {
		left = 4,
		right = 4,
		top = 4,
		bottom = 0,
	}
	config.inactive_pane_hsb = {
		saturation = 1,
		brightness = 0.9,
	}
	-- config.underline_position = -4 -- seems to mess with fonts
	-- config.underline_thickness = 2 -- weird

	config.font_size = 13.0
	-- wezterm ls-fonts --list-system
	config.font = wezterm.font {
		-- family = 'Liga Hack',
		family = 'FiraCode Nerd Font',
		-- family = 'FiraCode Nerd Font Mono',
		-- family = 'FiraCode Nerd Font Propo',
		weight = 'Medium',
	}

	config.color_scheme = 'rose-pine-moon'
	config.colors = theme.colors
	format_fancy(config)
	-- format_non_fancy(config)

	format_tabs()
end

return M
