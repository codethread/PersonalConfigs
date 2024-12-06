local wezterm = require 'wezterm' --[[@as Wezterm]]

local M = {}

local cache = {}

---comment
---@param file string
---@param opts? { no_cache?: boolean }
---@return table
local function readAll(file, opts)
	opts = opts or {}
	if not opts.no_cache and cache[file] then return cache[file] end
	local f = assert(io.open(file, 'rb'))
	local content = f:read '*all'
	f:close()
	cache[file] = content
	return content
end

function M.get_envs()
	-- local path = readAll('/etc/paths'):gsub('%s+', '')
	local envs_raw = readAll(wezterm.home_dir .. '/.config/envy/envs.json'):gsub('%s+', '')
	local envs = wezterm.json_parse(envs_raw)
	return envs
end

---@param config Config
function M.apply_to_config(config)
	config.set_environment_variables = M.get_envs()
	config.default_prog = { 'nu', '-l' }
	-- not sure if this will be annoying
	config.exit_behavior = 'CloseOnCleanExit'
end

return M
