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
-- 'tab': alt session

local config = wezterm.config_builder()

config.max_fps = 120

settings.apply_to_config(config)
ui.apply_to_config(config)
sessions.apply_to_config(config)
keymaps.apply_to_config(config)

print '____RELOADED____'
config.automatically_reload_config = false
return config
