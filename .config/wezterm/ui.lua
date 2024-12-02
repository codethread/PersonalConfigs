---@diagnostic disable: missing-fields
local wezterm = require 'wezterm' --[[@as Wezterm]]
local theme = require 'theme'
local utils = require 'utils'

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
		left = 2,
		right = 2,
		top = 2,
		bottom = 0,
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

	config.tab_bar_at_bottom = true
	config.use_fancy_tab_bar = false
	config.tab_max_width = 100 -- non-fancy
	config.color_scheme = 'rose-pine-moon'
	config.colors = theme.colors()
	config.window_frame = theme.window_frame()

	format_tabs()
end

return M
