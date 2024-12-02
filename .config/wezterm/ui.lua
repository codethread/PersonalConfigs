---@diagnostic disable: missing-fields
local wezterm = require 'wezterm' --[[@as Wezterm]]
local theme = require 'theme'

local M = {}

-- tmux status
-- this seems to tick every two second or on key down
wezterm.on('update-right-status', function(window, _)
	print(window:mux_window():get_title())
	local name = window:mux_window():get_workspace()
	local text = '  ' .. name .. '  '
	local SOLID_LEFT_ARROW = ''
	local ARROW_FOREGROUND = { Foreground = { Color = theme.colors().brights[3] } }
	local prefix = ''

	if window:leader_is_active() then
		prefix = ' ' .. utf8.char(0x1f30a) -- ocean wave
		SOLID_LEFT_ARROW = utf8.char(0xe0b2)
	end

	if window:active_tab():tab_id() ~= 0 then
		ARROW_FOREGROUND = { Foreground = { Color = theme.colors().brights[4] } }
	end -- arrow color based on if tab is first pane

	window:set_left_status(wezterm.format {
		-- { Background = { Color = '#b7bdf8' } },
		{ Text = text },
		-- { Text = prefix },
		-- ARROW_FOREGROUND,
		-- { Text = SOLID_LEFT_ARROW },
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
			return {
				-- { Background = { Color = 'blue' } },
				{ Text = ' ' .. title .. ' ' },
			}
		end
		return ' ' .. title .. ' '
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
		active_titlebar_bg = theme.colors().background,
		inactive_titlebar_bg = theme.colors().background,
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
		local buzz = {}

		if dpi == SIZE.CRISP then
			overrides.harfbuzz_features = {
				table.unpack(buzz),
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
		else
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
		wezterm.GLOBAL.overrides_hash = overrides_hash
		print('font changes', overrides_hash)
		window:set_config_overrides(overrides)
		print(window:effective_config().harfbuzz_features)
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
	config.colors = theme.colors()
	format_fancy(config)
	-- format_non_fancy(config)

	format_tabs()
end

return M
