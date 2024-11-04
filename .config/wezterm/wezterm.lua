-- examples:
-- https://github.com/emretuna/.dotfiles/tree/main/wezterm/.config/wezterm

-- Pull in the wezterm API
local wezterm = require 'wezterm'
local helpers = require 'helpers'

-- This will hold the configuration.
local config = wezterm.config_builder()

config.default_prog = { '/Users/codethread/.local/bin/nu', '-l' }
config.font = wezterm.font(helpers.font)

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'rose-pine-moon'
config.line_height = 1.2

config.enable_tab_bar = false
config.window_decorations = 'RESIZE'

config.leader = { key = 'b', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
	-- Attach to muxer
	{
		key = 'a',
		mods = 'LEADER',
		action = wezterm.action.AttachDomain 'unix',
	},
	-- Detach from muxer
	{
		key = 'd',
		mods = 'LEADER',
		action = wezterm.action.DetachDomain { DomainName = 'unix' },
	},
	{
		key = 'p',
		mods = 'LEADER',
		action = wezterm.action.ActivateCommandPalette,
	},
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
}

local bar = wezterm.plugin.require 'https://github.com/adriankarlen/bar.wezterm'
bar.apply_to_config(config)
-- and finally, return the configuration to wezterm
return config
