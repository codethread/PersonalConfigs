local wezterm = require 'wezterm' --[[@as Wezterm]]
local act = wezterm.action
local utils = require 'utils'

-- Function to find duplicates
local function findDuplicates(list)
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

local function create_keymaps(keymaps)
	local keys = {}
	-- Iterate over the keymaps table
	for mods, keymap in pairs(keymaps) do
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

	return keys
end

local M = {}

---comment
---@param config Config
function M.apply_to_config(config)
	local smart_splits = wezterm.plugin.require 'https://github.com/mrjones2014/smart-splits.nvim'

	config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }

	-- https://wezfurlong.org/wezterm/config/keys.html#configuring-key-assignments
	local keymaps = {
		CMD = {
			[','] = {
				action = act.SwitchToWorkspace {
					name = 'wez',
					spawn = {
						cwd = utils.home 'PersonalConfigs/.config/wezterm',
						args = { utils.bin 'nvim', 'wezterm.lua' },
					},
				},
			},
		},
		LEADER = {
			[';'] = act.ActivateCommandPalette,

			[' '] = {
				action = act.TogglePaneZoomState,
			},

			['I'] = {
				action = wezterm.action_callback(function(win, pan)
					win:toast_notification('wezterm', 'Updating...', nil, 4000)
					-- print(wezterm.plugin.list()) -- will list the plugin repos.
					wezterm.plugin.update_all()
					win:toast_notification('wezterm', '✨ Updated', nil, 4000)
					wezterm.reload_configuration()
				end),
			},

			['c'] = {
				action = act.SpawnTab 'CurrentPaneDomain',
			},
			['x'] = {
				action = act.CloseCurrentPane { confirm = true },
			},
			['n'] = {
				action = act.SpawnTab 'CurrentPaneDomain',
			},

			['|'] = {
				action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
			},
			['-'] = {
				action = act.SplitVertical { domain = 'CurrentPaneDomain' },
			},

			['b'] = {
				action = act.PaneSelect {
					mode = 'MoveToNewTab',
				},
			},

			['h'] = {
				action = act.ActivatePaneDirection 'Left',
			},
			['j'] = {
				action = act.ActivatePaneDirection 'Down',
			},
			['k'] = {
				action = act.ActivatePaneDirection 'Up',
			},
			['l'] = {
				action = act.ActivatePaneDirection 'Right',
			},

			['LeftArrow'] = {
				action = act.AdjustPaneSize { 'Left', 5 },
			},
			['RightArrow'] = {
				action = act.AdjustPaneSize { 'Right', 5 },
			},
			['DownArrow'] = {
				action = act.AdjustPaneSize { 'Down', 5 },
			},
			['UpArrow'] = {
				action = act.AdjustPaneSize { 'Up', 5 },
			},

			['{'] = {
				action = act.RotatePanes 'CounterClockwise',
			},
			['}'] = {
				action = act.RotatePanes 'Clockwise',
			},

			['S'] = {
				action = act.PaneSelect {
					mode = 'SwapWithActiveKeepFocus', -- 'SwapWithActive'
				},
			},
			['['] = {
				action = act.ActivateCopyMode,
			},
			w = {
				-- TODO figure out how to do this in workspace
				action = act.SwitchToWorkspace {
					name = 'work-web',
					-- args = { 'nush', 'work-start-work-wez' },
					spawn = {
						cwd = utils.home 'work/deals-light-ui',
						args = { 'testy' },
					},
				},
			},
			f = {
				action = require('sessions').sessionizer().show,
			},
			['p'] = {
				action = require('sessions').sessionizer().show_active,
			},
			['Tab'] = {
				action = require('sessions').sessionizer().switch_to_most_recent,
			},

			['Backspace'] = {
				action = act.CloseCurrentTab { confirm = false },
			},

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
							win:perform_action(
								act.CloseCurrentTab { confirm = false },
								t:active_pane()
							)
						end
					end
				end),
			},
		},

		['LEADER|CTRL'] = {
			['a'] = {
				action = act { SendString = '\x01' },
			},
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

	local keys = create_keymaps(keymaps)
	if not config.keys then config.keys = {} end

	-- print('Key:', config.keys)
	for _, key in ipairs(keys) do
		table.insert(config.keys, key)
	end

	wezterm.on('window-config-reloaded', function(window)
		local duplicates = findDuplicates(config.keys)
		for _, dup in ipairs(duplicates) do
			window:toast_notification('wezterm', 'Duplicate key: ' .. dup, nil, 4000)
		end
	end)
end

return M
