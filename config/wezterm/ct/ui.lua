---@diagnostic disable: missing-fields
local wezterm = require 'wezterm' --[[@as Wezterm]]
local theme = require 'ct.theme'
local _ = require '_'

local M = {}

local SIZE = {
	LARGE = 'large',
	CRISP = 'CRISP',
}

---Update Ui elements that require screen info (only assesible in callback at
---this time)
---@param window Window
local function update_font_config(window)
	if not window:is_focused() then return end

	local gui = wezterm.gui
	if gui then
		local dpi = gui.screens().main.effective_dpi > 100 and SIZE.CRISP or SIZE.LARGE

		local overrides = window:get_config_overrides() or {}
		if dpi == SIZE.CRISP then
			overrides.font_size = 14.0
		else
			overrides.font_size = 16.0
			-- overrides.dpi = 109
		end

		overrides.command_palette_font_size = overrides.font_size + 2

		local overrides_hash = wezterm.json_encode {
			[dpi] = _.obj.pick(overrides, { 'font_size', 'command_palette_font_size' }),
		}

		if wezterm.GLOBAL.overrides_hash == overrides_hash then return end
		print('font changes', overrides)
		wezterm.GLOBAL.overrides_hash = overrides_hash
		window:set_config_overrides(overrides)
	end
end

---@param config Config
function M.apply_to_config(config)
	config.max_fps = 120
	config.window_decorations = 'RESIZE'
	-- config.tab_and_split_indices_are_zero_based = true
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
	-- config.underline_thickness = 2 -- weird

	config.font_size = 14.0
	config.line_height = 1.2
	config.underline_position = -4

	-- wezterm ls-fonts --list-system

	-- config.font = wezterm.font('JetBrains Mono', { weight = 'DemiBold' })
	-- config.font = wezterm.font { family = 'FiraCode Nerd Font', weight = 'DemiBold' }
	config.font = wezterm.font 'Fira Code'
	config.harfbuzz_features = {
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

	config.color_scheme = 'rose-pine-moon'
	config.colors = theme.colors

	config.command_palette_bg_color = theme.palette.overlay
	config.command_palette_fg_color = theme.colors.foreground

	M.format.fancy(config)
end

wezterm.on('window-focus-changed', update_font_config)
wezterm.on('window-config-reloaded', update_font_config)

wezterm.on('format-tab-title', function(tab)
	-- It prefers the title that was set via `tab:set_title()` but falls back
	-- to the title of the active pane in that tab.
	--
	-- https://wezfurlong.org/wezterm/config/lua/window-events/format-tab-title.html

	local title
	do -- create tab title
		local tt = tab.tab_title
		local is_zoomed = tab.active_pane.is_zoomed and '+ ' or ''
		if tt and #tt > 0 then
			title = tt .. is_zoomed
		else
			title = tab.active_pane.title .. is_zoomed
		end
	end

	return wezterm.format {
		{ Attribute = { Intensity = tab.is_active and 'Bold' or 'Normal' } },
		{ Text = title },
	}
end)

wezterm.on('update-right-status', function(window, _)
	local ok, name = pcall(function() return window:mux_window():get_workspace() end)
	if not ok then
		-- can fail when a window is closed
		return
	end
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

---https://wezfurlong.org/wezterm/config/appearance.html#native-fancy-tab-bar-appearance
---@package
M.format = {
	---@param config Config
	fancy = function(config)
		-- config.show_close_tab_button_in_tabs = false
		config.tab_bar_at_bottom = false
		config.use_fancy_tab_bar = true
		config.window_frame = {
			font = config.font,
			font_size = config.font_size,
			active_titlebar_bg = theme.colors.background,
			inactive_titlebar_bg = theme.colors.background,
		}
	end,
	---@param config Config
	retro = function(config)
		config.tab_bar_at_bottom = false
		config.tab_max_width = 100 -- non-fancy
		config.use_fancy_tab_bar = false
	end,
}

return M
