---@diagnostic disable: missing-fields
-- examples:
-- https://github.com/emretuna/.dotfiles/tree/main/wezterm/.config/wezterm
-- https://github.com/joshmedeski/dotfiles/blob/main/.config/wezterm/wezterm.lua
-- https://raw.githubusercontent.com/dragonlobster/wezterm-config/main/wezterm.lua
-- https://github.com/gonstoll/wezterm-types/blob/main/wezterm.lua

-- Pull in the wezterm API
local wezterm = require 'wezterm' --[[@as Wezterm]]
local helpers = require 'helpers'

local config = wezterm.config_builder()

local smart_splits = wezterm.plugin.require 'https://github.com/mrjones2014/smart-splits.nvim'

local function home(path) return os.getenv 'HOME' .. '/' .. path end
local function bin(b) return '/opt/homebrew/bin/' .. b end
config.default_prog = { home '.local/bin/nu', '-l' }

-- config.line_height = 1.3
config.line_height = 1.5
config.font_size = 13.0
config.underline_position = -4
config.font = wezterm.font_with_fallback {
	{
		family = helpers.font,
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

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'rose-pine-moon'

config.window_decorations = 'RESIZE'
-- config.enable_tab_bar = false
-- tab bar
-- config.underline_thickness = 2
config.tab_bar_at_bottom = true
-- config.use_fancy_tab_bar = false
-- config.tab_and_split_indices_are_zero_based = true

local theme = wezterm.plugin.require('https://github.com/neapsix/wezterm').moon
-- TODO: steal the colors for task bar
-- config.colors = theme.colors()
config.window_frame = theme.window_frame()
config.hide_tab_bar_if_only_one_tab = true

config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
	{
		key = ',',
		mods = 'CMD',
		action = wezterm.action.SwitchToWorkspace {
			name = 'wez',
			spawn = {
				cwd = home 'PersonalConfigs/.config/wezterm',
				args = { bin 'nvim', 'wezterm.lua' },
			},
		},
	},
	{
		key = '1',
		mods = 'LEADER',
		action = wezterm.action.SwitchToWorkspace {
			name = home 'PersonalConfigs',
			spawn = { cwd = home 'PersonalConfigs' },
		},
	},
	{
		key = '2',
		mods = 'LEADER',
		action = wezterm.action.SwitchToWorkspace {
			name = home 'work/deals-light-ui',
			spawn = { cwd = home 'work/deals-light-ui' },
		},
	},
	-- {
	-- 	key = 'p',
	-- 	mods = 'LEADER',
	-- 	action = wezterm.action.ActivateCommandPalette,
	-- },
	{
		key = 'P',
		mods = 'CTRL',
		action = wezterm.action.QuickSelectArgs {
			patterns = {
				'https?://\\S+',
				'/Users/\\S+',
			},
		},
	},
	{
		key = ' ',
		mods = 'LEADER',
		action = wezterm.action.TogglePaneZoomState,
	},
	{
		mods = 'LEADER',
		key = 'c',
		action = wezterm.action.SpawnTab 'CurrentPaneDomain',
	},
	{
		mods = 'LEADER',
		key = 'x',
		action = wezterm.action.CloseCurrentPane { confirm = true },
	},
	{
		mods = 'LEADER',
		key = 'b',
		action = wezterm.action.ActivateTabRelative(-1),
	},
	{
		mods = 'LEADER',
		key = 'n',
		action = wezterm.action.ActivateTabRelative(1),
	},
	{
		mods = 'LEADER',
		key = '|',
		action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
	},
	{
		mods = 'LEADER',
		key = '-',
		action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
	},
	{
		mods = 'LEADER',
		key = 'h',
		action = wezterm.action.ActivatePaneDirection 'Left',
	},
	{
		mods = 'LEADER',
		key = 'j',
		action = wezterm.action.ActivatePaneDirection 'Down',
	},
	{
		mods = 'LEADER',
		key = 'k',
		action = wezterm.action.ActivatePaneDirection 'Up',
	},
	{
		mods = 'LEADER',
		key = 'l',
		action = wezterm.action.ActivatePaneDirection 'Right',
	},
	{
		mods = 'LEADER',
		key = 'LeftArrow',
		action = wezterm.action.AdjustPaneSize { 'Left', 5 },
	},
	{
		mods = 'LEADER',
		key = 'RightArrow',
		action = wezterm.action.AdjustPaneSize { 'Right', 5 },
	},
	{
		mods = 'LEADER',
		key = 'DownArrow',
		action = wezterm.action.AdjustPaneSize { 'Down', 5 },
	},
	{
		mods = 'LEADER',
		key = 'UpArrow',
		action = wezterm.action.AdjustPaneSize { 'Up', 5 },
	},
}

-- tmux status
wezterm.on('update-right-status', function(window, _)
	local SOLID_LEFT_ARROW = ''
	local ARROW_FOREGROUND = { Foreground = { Color = '#c6a0f6' } }
	local prefix = ''

	if window:leader_is_active() then
		prefix = ' ' .. utf8.char(0x1f30a) -- ocean wave
		SOLID_LEFT_ARROW = utf8.char(0xe0b2)
	end

	if window:active_tab():tab_id() ~= 0 then
		ARROW_FOREGROUND = { Foreground = { Color = '#1e2030' } }
	end -- arrow color based on if tab is first pane

	window:set_left_status(wezterm.format {
		{ Background = { Color = '#b7bdf8' } },
		{ Text = prefix },
		ARROW_FOREGROUND,
		{ Text = SOLID_LEFT_ARROW },
	})
end)

-- }

-- local bar = wezterm.plugin.require 'https://github.com/adriankarlen/bar.wezterm'
-- bar.apply_to_config(config)
-- and finally, return the configuration to wezterm

local sessionizer = wezterm.plugin.require 'https://github.com/mikkasendke/sessionizer.wezterm'
sessionizer.apply_to_config(config)
sessionizer.config = {
	paths = {
		home 'dev',
		home 'work',
		home '.local/share/nvim/lazy',
		home '.local/share/nvim/mason/packages',
	},
	command_options = {
		fd_path = bin 'fd',
		max_depth = 3,
	},
}
smart_splits.apply_to_config(config, {
	-- the default config is here, if you'd like to use the default keys,
	-- you can omit this configuration table parameter and just use
	-- smart_splits.apply_to_config(config)

	-- directional keys to use in order of: left, down, up, right
	-- direction_keys = { 'h', 'j', 'k', 'l' },
	-- if you want to use separate direction keys for move vs. resize, you
	-- can also do this:
	direction_keys = {
		move = { 'h', 'j', 'k', 'l' },
		resize = { 'LeftArrow', 'DownArrow', 'UpArrow', 'RightArrow' },
	},
	-- modifier keys to combine with direction_keys
	modifiers = {
		move = 'CTRL', -- modifier to use for pane movement, e.g. CTRL+h to move left
		resize = 'META', -- modifier to use for pane resize, e.g. META+h to resize to the left
	},
})
return config
