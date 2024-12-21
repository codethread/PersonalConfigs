---@diagnostic disable: param-type-mismatch
local wezterm = require 'wezterm' --[[@as Wezterm]]
local actions = require 'ct.actions'
local _ = require '_'
local act = wezterm.action
local utils = require 'ct.utils'
local switch_workspace = require('ct.sessions').switch_workspace
local switch_last_workspace = require('ct.sessions').switch_last_workspace
local sessionizer = require('ct.sessions').sessionizer

local M = {}

-- https://wezfurlong.org/wezterm/config/keys.html#configuring-key-assignments
local keymaps = {
	CMD = {
		[','] = {
			action = switch_workspace {
				name = 'wez',
				spawn = {
					cwd = utils.home 'PersonalConfigs/.config/wezterm',
					args = { utils.bin 'nvim', 'wezterm.lua' },
				},
			},
		},
	},
	LEADER = {
		['/'] = { action = actions.open_file_in_nvim },
		['?'] = { action = actions.quick_select_file_for_editor },
		[';'] = act.ActivateCommandPalette,

		[' '] = {
			action = wezterm.action_callback(function(win, pane)
				-- if only one pane, create a split instead of doing nothing
				if #win:active_tab():panes() > 1 then
					return win:perform_action(act.TogglePaneZoomState, pane)
				else
					return win:perform_action(act.SplitHorizontal { domain = 'CurrentPaneDomain' }, pane)
				end
			end),
		},

		['I'] = {
			action = wezterm.action_callback(function(win, pan)
				-- must be set to "notification type > alerts" in macos
				win:toast_notification('wezterm', 'Updating...', nil, 4000)
				print(wezterm.plugin.list()) -- will list the plugin repos.
				wezterm.plugin.update_all()
				win:toast_notification('wezterm', '✨ Updated', nil, 4000)
				wezterm.reload_configuration()
			end),
		},

		['c'] = { action = act.SpawnTab 'CurrentPaneDomain' },
		['x'] = { action = act.CloseCurrentPane { confirm = true } },
		['n'] = { action = act.SpawnTab 'CurrentPaneDomain' },

		['|'] = { action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
		['-'] = { action = act.SplitVertical { domain = 'CurrentPaneDomain' } },

		['b'] = {
			action = act.PaneSelect { mode = 'MoveToNewTab' },
		},

		['h'] = { action = act.ActivatePaneDirection 'Left' },
		['j'] = { action = act.ActivatePaneDirection 'Down' },
		['k'] = { action = act.ActivatePaneDirection 'Up' },
		['l'] = { action = act.ActivatePaneDirection 'Right' },

		['LeftArrow'] = { action = act.AdjustPaneSize { 'Left', 5 } },
		['RightArrow'] = { action = act.AdjustPaneSize { 'Right', 5 } },
		['DownArrow'] = { action = act.AdjustPaneSize { 'Down', 5 } },
		['UpArrow'] = { action = act.AdjustPaneSize { 'Up', 5 } },

		['{'] = { action = act.RotatePanes 'CounterClockwise' },
		['}'] = { action = act.RotatePanes 'Clockwise' },

		['s'] = {
			action = act.PaneSelect {
				mode = 'SwapWithActiveKeepFocus', -- 'SwapWithActive'
			},
		},
		['['] = { action = act.ActivateCopyMode },
		['w'] = { action = actions.runWorkProject },
		['W'] = {
			action = switch_workspace {
				name = 'work-native',
				spawn = {
					args = { 'fe-native-start' },
				},
			},
		},
		['f'] = { action = sessionizer().show },
		['p'] = { action = sessionizer().show_active },
		['Tab'] = { action = sessionizer().switch_to_most_recent },
		['e'] = { action = actions.open_scrollback_in_nvim },

		['Backspace'] = { action = act.CloseCurrentTab { confirm = false } },

		-- ['o'] = {
		-- 	-- close other panes
		-- 	action = wezterm.action_callback(function(win, pane)
		-- 		local tab = win:active_tab()
		-- 		for _, p in ipairs(tab:panes()) do
		-- 			if p:pane_id() ~= pane:pane_id() then
		-- 				p:activate()
		-- 				win:perform_action(act.CloseCurrentPane { confirm = false }, p)
		-- 			end
		-- 		end
		-- 	end),
		-- },

		['o'] = {
			-- close other windows
			action = wezterm.action_callback(function(win, pane)
				local tab = win:active_tab()
				for _, t in ipairs(pane:window():tabs()) do
					if t:tab_id() ~= tab:tab_id() then
						t:activate()
						win:perform_action(act.CloseCurrentTab { confirm = false }, t:active_pane())
					end
				end
			end),
		},
	},

	['CTRL'] = {
		['Tab'] = { action = switch_last_workspace() },
	},

	['SHIFT'] = {
		['UpArrow'] = { action = act.ScrollToPrompt(-1) },
		['DownArrow'] = { action = act.ScrollToPrompt(1) },
	},

	['LEADER|CTRL'] = {
		-- assumes leader is also ctrl-a
		-- mimics tmux behaviour of sending ctrl-a if hit twice, e.g hit
		-- ctrl-a ctrl-a to send a single ctrl-a to vim
		['a'] = { action = act { SendString = '\x01' } },
	},
}

---@param config Config
function M.apply_to_config(config)
	wezterm.on('window-config-reloaded', M.notify_on_key_clash)

	config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }

	config.mouse_bindings = {
		{
			-- event = { Down = { streak = 3, button = 'Left' } },
			event = { Down = { streak = 1, button = 'Right' } },
			action = wezterm.action.SelectTextAtMouseCursor 'SemanticZone',
			mods = 'NONE',
		},
	}

	local smart_splits = wezterm.plugin.require 'https://github.com/mrjones2014/smart-splits.nvim'
	smart_splits.apply_to_config(config, { log_level = 'warn' })

	M.create_keymaps(config, keymaps)
end

---Create wezterm keymaps from custom mappings
---@package
---@param config Config
---@param custom_keys table
---@return table
function M.create_keymaps(config, custom_keys)
	local keys = {}

	-- Iterate over the keymaps table
	for mods, keymap in pairs(custom_keys) do
		for key, values in pairs(keymap) do
			local entry = {}

			-- TODO this doesn't really work as most of the act.* create tables
			if type(values) == 'table' then
				entry = values
			else
				-- allow passing just a function
				entry.action = values
			end

			entry.key = key
			entry.mods = mods

			table.insert(keys, entry)
		end
	end

	for _, key in ipairs(keys) do
		table.insert(config.keys or {}, key)
	end
	return config
end

---Notify if any of my keymaps overlap
---@param window Window
function M.notify_on_key_clash(window)
	local config = window:effective_config()
	local duplicates = M.findKeymapDuplicates(config.keys)
	for _, dup in ipairs(duplicates) do
		window:toast_notification('wezterm', 'Duplicate key: ' .. dup, nil, 4000)
	end
end

---Function to find duplicates
---@package
function M.findKeymapDuplicates(list)
	local occurrences = {}
	local duplicates = {}

	for _, item in ipairs(list) do
		-- Create a unique identifier for the table
		local identifier = item.mods .. ' +  ' .. item.key

		-- Count occurrences
		if not occurrences[identifier] then
			occurrences[identifier] = 1
		else
			occurrences[identifier] = occurrences[identifier] + 1
		end
	end

	-- Collect duplicates
	for identifier, count in pairs(occurrences) do
		if count > 1 then table.insert(duplicates, identifier) end
	end

	return duplicates
end

return M
