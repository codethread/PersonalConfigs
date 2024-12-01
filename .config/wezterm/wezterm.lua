---@diagnostic disable: missing-fields
-- examples:
-- https://github.com/emretuna/.dotfiles/tree/main/wezterm/.config/wezterm
-- https://github.com/joshmedeski/dotfiles/blob/main/.config/wezterm/wezterm.lua
-- https://raw.githubusercontent.com/dragonlobster/wezterm-config/main/wezterm.lua
-- https://github.com/gonstoll/wezterm-types/blob/main/wezterm.lua

-- Pull in the wezterm API
local wezterm = require 'wezterm' --[[@as Wezterm]]
local keymaps = require 'keymaps'
local settings = require 'settings'
local ui = require 'ui'
local sessions = require 'sessions'

-- NOTE: debugging
-- https://wezfurlong.org/wezterm/troubleshooting.html

-- TODO: Missing tmux commands
-- 'o': only current and close other tabs
-- 'backspace': close tab
-- 'tab': alt session

local config = wezterm.config_builder()

config.max_fps = 120

settings.apply_to_config(config)
ui.apply_to_config(config)
sessions.apply_to_config(config)
keymaps.apply_to_config(config)

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

print '____RELOADED____'
return config
