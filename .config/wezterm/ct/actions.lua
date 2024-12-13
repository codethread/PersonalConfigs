local wezterm = require 'wezterm' --[[@as Wezterm]]
local utils = require 'ct.utils'
local _ = require '_'

local act = wezterm.action

local M = {}

M.runWorkProject = wezterm.action_callback(function(w, p)
	local TARGET = 'work-web'
	local sessions = wezterm.mux.get_workspace_names()

	local running_session = _.list.find(sessions, function(s) return _.str(s):starts_with(TARGET) end)

	if running_session then
		return w:perform_action(
			act.SwitchToWorkspace {
				name = running_session,
			},
			p
		)
	end

	local projects = { 'deals-light-ui', 'fe-review', 'fe-native' }

	local choices = _.list.map(projects, function(label) return { label = label } end)

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
