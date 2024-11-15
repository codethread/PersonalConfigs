local wezterm = require 'wezterm' --[[@as Wezterm]]
local utils = require 'utils'

local M = {}

local projects = {
	{ 1, utils.home 'PersonalConfigs' },
	{ 2, utils.home 'work/deals-light-ui' },
	{ 3, utils.home 'work/fe-review' },
	{ 4, utils.home 'work/services/flexi-points-app' },
	{ 5, utils.home 'work/fe-native' },
	{ 6, utils.home 'dev/projects/git-tools' },
	{ 7, utils.home 'work/libraries/inspect-my-package' },
	{ 8, utils.home 'workfiles' },
	{ 9, utils.home 'work/services/protobuf' },
}

function M.sessionizer()
	return wezterm.plugin.require 'https://github.com/mikkasendke/sessionizer.wezterm'
end

---comment
---@param config Config
function M.apply_to_config(config)
	if not config.keys then config.keys = {} end
	for _, project in ipairs(projects) do
		table.insert(config.keys, {
			key = tostring(project[1]),
			mods = 'LEADER',
			action = wezterm.action.SwitchToWorkspace {
				name = project[2],
				spawn = { cwd = project[2] },
			},
		})
	end

	local sessionizer = M.sessionizer()

	sessionizer.apply_to_config(
		config,
		true -- disable default bindings
	)
	sessionizer.config = {
		paths = {
			utils.home 'dev',
			utils.home 'work',
			utils.home '.local/share/nvim/lazy',
			utils.home '.local/share/nvim/mason/packages',
		},
		command_options = {
			fd_path = utils.bin 'fd',
			max_depth = 3,
		},
	}
end

return M
