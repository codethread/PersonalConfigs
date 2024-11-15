local wezterm = require 'wezterm' --[[@as Wezterm]]

local M = {}

local function readAll(file)
	local f = assert(io.open(file, 'rb'))
	local content = f:read '*all'
	f:close()
	return content
end

local function get_envs()
	-- local path = readAll('/etc/paths'):gsub('%s+', '')
	local envs_raw = readAll(wezterm.home_dir .. '/.config/envy/envs.json'):gsub('%s+', '')
	local envs = wezterm.json_parse(envs_raw)
	return envs
end

---@param config Config
function M.apply_to_config(config)
	config.set_environment_variables = get_envs()
	config.default_prog = { 'nu', '-l' }
	-- not sure if this will be annoying
	config.exit_behavior = 'CloseOnCleanExit'
end

return M
