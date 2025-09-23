-- :module: Main WezTerm terminal emulator configuration
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
local config = wezterm.config_builder()

---Not present in wezterm, but if using things from luarocks, they may expect this
debug = require 'debug'

local ok, config_or_err = pcall(function()
	local utils = require 'ct.utils'

	config.set_environment_variables = utils.get_envs()
	config.default_prog = { 'nu', '-l' }
	-- not sure if this will be annoying
	config.exit_behavior = 'CloseOnCleanExit'

	require 'ct.events'
	local file_handlers = require 'ct.filehandlers'
	file_handlers.apply_to_config(config)
	local ui = require 'ct.ui'
	ui.apply_to_config(config)
	local sessions = require 'ct.sessions'
	sessions.apply_to_config(config)
	local keymaps = require 'ct.keymaps'
	keymaps.apply_to_config(config)

	return config
end)

-- config_or_err.automatically_reload_config = false

if not ok then
	wezterm.log_error(config_or_err --[[@as string]])

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

---@class WezGlobal
---@field sessions? { current: 'a'|'b', a: string, b: string } Stored sessions for toggling
---@field overrides_hash? string Stored overrides for checking changes between update events

---@class Wezterm
---@diagnostic disable-next-line: duplicate-doc-field
---@field GLOBAL WezGlobal
