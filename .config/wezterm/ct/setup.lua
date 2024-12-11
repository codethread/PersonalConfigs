local wezterm = require 'wezterm' --[[@as Wezterm]]
local utils = require 'ct.utils'

local setup = function()
	local config = wezterm.config_builder()

	config.max_fps = 120
	config.set_environment_variables = utils.get_envs()
	config.default_prog = { 'nu', '-l' }
	-- not sure if this will be annoying
	config.exit_behavior = 'CloseOnCleanExit'

	Try {
		function()
			local ui = require 'ct.ui'
			ui.apply_to_config(config)
		end,
		function()
			local sessions = require 'ct.sessions'
			sessions.apply_to_config(config)
		end,
		function()
			local keymaps = require 'ct.keymaps'
			keymaps.apply_to_config(config)
		end,
	}

	return config
end

return { setup = setup }
