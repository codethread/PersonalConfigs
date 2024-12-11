-- NOTE: examples:
-- https://github.com/emretuna/.dotfiles/tree/main/wezterm/.config/wezterm
-- https://github.com/joshmedeski/dotfiles/blob/main/.config/wezterm/wezterm.lua
-- https://raw.githubusercontent.com/dragonlobster/wezterm-config/main/wezterm.lua
-- https://github.com/gonstoll/wezterm-types/blob/main/wezterm.lua

-- NOTE: debugging
-- https://wezfurlong.org/wezterm/troubleshooting.html

-- TODO: Missing:
-- 'tab': alt session
-- open in vim
-- fuzzy select url from other pane (e.g to open failed tests)
-- rename tab to something better (e.g command, not full path)

print '--|  LOADING   |--'

local wezterm = require 'wezterm' --[[@as Wezterm]]

local _, err = pcall(function() require('ct.globals').setup() end)
if err then wezterm.log_error(err) end

local ok, config_or_err = pcall(function() return require('ct.setup').setup() end)

-- config_or_err.automatically_reload_config = false

if not ok then
	wezterm.log_error(config_or_err --[[@as string]])
	local config = wezterm.config_builder()

	config.max_fps = 120
	config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }

	---@diagnostic disable: missing-fields
	config.keys = {
		{

			key = ';',
			mods = 'LEADER',
			---@diagnostic disable-next-line: assign-type-mismatch
			action = wezterm.action.ActivateCommandPalette,
		},
		{
			key = 'I',
			mods = 'LEADER',
			---@diagnostic disable-next-line: assign-type-mismatch
			action = wezterm.action_callback(function(win)
				-- must be set to "notification type > alerts" in macos
				win:toast_notification('wezterm', 'Updating...', nil, 4000)
				print(wezterm.plugin.list()) -- will list the plugin repos.
				wezterm.plugin.update_all()
				win:toast_notification('wezterm', '✨ Updated', nil, 4000)
				wezterm.reload_configuration()
			end),
		},
	}
	print '--| FALLBACK LOADED |--'
	return config
else
	print '--| LOADED |--'
	return config_or_err
end
