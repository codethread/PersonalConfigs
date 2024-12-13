local wezterm = require 'wezterm' --[[@as Wezterm]]
local _ = require '_'
local M = {}

function M.home(path) return os.getenv 'HOME' .. '/' .. path end
function M.bin(b) return '/opt/homebrew/bin/' .. b end

---@param current 'home' | 'work'
---@return boolean
function M.machine(current)
	local user = M.get_envs().CT_USER
	print('user: ', user)
	return current == user
end

---@param fullPath string
---@return string
function M.getFilename(fullPath)
	-- Find the last occurrence of a slash
	local filename = fullPath:match '([^/\\]+)$'
	return filename
end

function M.get_envs()
	local target = wezterm.home_dir .. '/.config/envy/envs.json'
	local raw = _.fs.readfile(target)
	if not raw then error('could not load envs file: ' .. target) end
	local envs = wezterm.json_parse(raw:trim():to_string())
	if not envs then error('could not load envs file: ' .. target) end
	return envs
end

return M
