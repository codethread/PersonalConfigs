local wezterm = require 'wezterm' --[[@as Wezterm]]
local Str = require 'ct.collections.Str'
local Arr = require 'ct.collections.Arr'
local utils = require 'ct.utils'

local act = wezterm.action

local M = {}

M.runWorkProject = wezterm.action_callback(function(w, p)
	local TARGET = 'work-web'
	local sessions = Arr(wezterm.mux.get_workspace_names())

	local running_session = sessions:find(function(s) return Str(s):starts_with(TARGET) end)

	if running_session then
		return w:perform_action(
			act.SwitchToWorkspace {
				name = running_session,
			},
			p
		)
	end

	local projects = Arr { 'deals-light-ui', 'fe-review', 'fe-native' }

	local choices = projects:map(function(label) return { label = label } end)

	w:perform_action(
		act.InputSelector {
			fuzzy = true,
			fuzzy_description = wezterm.format {
				{ Attribute = { Intensity = 'Bold' } },
				{ Foreground = { AnsiColor = 'Fuchsia' } },
				{ Text = 'foo bar' },
			},
			choices = choices,
			action = wezterm.action_callback(
				function(w1, p1, _, line)
					w1:perform_action(
						act.SwitchToWorkspace {
							name = TARGET .. '-' .. line,
							spawn = {
								cwd = utils.home('work/' .. line),
								args = { 'testy' },
							},
						},
						p1
					)
				end
			),
		},
		p
	)
end)

return M
