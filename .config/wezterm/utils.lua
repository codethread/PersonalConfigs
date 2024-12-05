local M = {}

function M.home(path) return os.getenv 'HOME' .. '/' .. path end
function M.bin(b) return '/opt/homebrew/bin/' .. b end

---@param current 'home' | 'work'
---@return boolean
function M.machine(current)
	local user = require('settings').get_envs().CT_USER
	print { user = user }
	return current == user
end

---@param fullPath string
---@return string
function M.getFilename(fullPath)
	-- Find the last occurrence of a slash
	local filename = fullPath:match '([^/\\]+)$'
	return filename
end

return M
